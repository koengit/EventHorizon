-- | Multi-user game abstraction for EventHorizon.
--
-- Separates shared 3D world state ('WorldScene') from per-player presentation
-- ('PlayerView': camera + HUD), while preserving EventHorizon's pure event-driven
-- architecture.
module Game.EventHorizon.Multi
  ( -- * Simulation Clock
    Tick
  , ticksPerSecond

    -- * Multi-Player Input & Events
  , InputEvent (..)
  , MultiEvent (..)

    -- * Shared World & Per-Player Views
  , WorldScene (..)
  , defaultWorldScene
  , PlayerView (..)
  , defaultPlayerView
  , combineScene

    -- * Scoped Commands
  , MultiCommand (..)

    -- * Complete Multi-User Game Specification
  , Game (..)

    -- * Helpers
  , canonicalPair
  , singlePlayerGame
  ) where

import Data.Word (Word64)
import Game.EventHorizon.Math
import Game.EventHorizon.Scene
import Game.EventHorizon.Types

-- | Discrete simulation tick index (at 'ticksPerSecond' Hz).
type Tick = Word64

-- | Fixed physics and networking simulation rate (60 Hz).
ticksPerSecond :: Int
ticksPerSecond = 60

-- | Player-originated input event delivered inside 'PlayerInput'.
data InputEvent bid
  = InKeyDown !Key
  | InKeyUp !Key
  | InMouseDown !MouseButton !(Maybe (bid, Vec3))
  | InMouseUp !MouseButton
  deriving (Eq, Show)

-- | Events that wake a multi-user pure game update function.
-- Parameterized by player ID @pid@, body ID @bid@, and timer ID @tid@.
data MultiEvent pid bid tid
  = PlayerJoined !pid
  | PlayerLeft !pid
  | PlayerInput !pid !(InputEvent bid)
  | WorldCollisionStart !bid !bid
  | WorldCollisionEnd !bid !bid
  | WorldTimerFired !tid
  deriving (Eq, Show)

-- | Shared 3D world state common to all connected players.
data WorldScene bid tid = WorldScene
  { wsEpoch :: !Int
  , wsGravity :: !Vec3
  , wsBackground :: !Color
  , wsFog :: !(Maybe (Color, Double, Double))
  , wsLights :: ![Light]
  , wsTree :: !(SceneTree bid)
  , wsTimers :: ![Timer tid]
  }
  deriving (Eq, Show)

-- | Default shared world scene with standard gravity and lighting.
defaultWorldScene :: WorldScene bid tid
defaultWorldScene =
  WorldScene
    { wsEpoch = sceneEpoch defaultScene
    , wsGravity = sceneGravity defaultScene
    , wsBackground = sceneBackground defaultScene
    , wsFog = sceneFog defaultScene
    , wsLights = sceneLights defaultScene
    , wsTree = sceneTree defaultScene
    , wsTimers = sceneTimers defaultScene
    }

-- | Per-player camera and HUD overlay rendered on top of a shared 'WorldScene'.
data PlayerView bid = PlayerView
  { pvCamera :: !(Camera bid)
  , pvHud :: ![HudElement]
  }
  deriving (Eq, Show)

-- | Default player view with fixed overview camera and empty HUD.
defaultPlayerView :: PlayerView bid
defaultPlayerView =
  PlayerView
    { pvCamera = sceneCamera defaultScene
    , pvHud = sceneHud defaultScene
    }

-- | Combine a shared 'WorldScene' and a player's 'PlayerView' into a full 'Scene'.
combineScene :: WorldScene bid tid -> PlayerView bid -> Scene bid tid
combineScene ws pv =
  Scene
    { sceneEpoch = wsEpoch ws
    , sceneGravity = wsGravity ws
    , sceneBackground = wsBackground ws
    , sceneFog = wsFog ws
    , sceneCamera = pvCamera pv
    , sceneLights = wsLights ws
    , sceneTree = wsTree ws
    , sceneTimers = wsTimers ws
    , sceneHud = pvHud pv
    }

-- | One-shot commands scoped either to the shared world or to a single player.
data MultiCommand pid bid
  = WorldCmd !(Command bid)
  | PlayerCmd !pid !(Command bid)
  deriving (Eq, Show)

-- | Complete pure specification of a multi-user EventHorizon game.
data Game pid bid tid world = Game
  { gameInit :: world
  , gameWorld :: world -> WorldScene bid tid
  , gameView :: pid -> world -> PlayerView bid
  , gameStep :: MultiEvent pid bid tid -> Snapshot bid -> world -> (world, [MultiCommand pid bid])
  }

-- | Order a pair of body IDs canonically so collision handling is order-independent.
canonicalPair :: Ord bid => bid -> bid -> (bid, bid)
canonicalPair a b
  | a <= b = (a, b)
  | otherwise = (b, a)

-- | Lift an existing single-player game into a 1-player 'Game'.
singlePlayerGame
  :: world
  -> (world -> Scene bid tid)
  -> (Event bid tid -> Snapshot bid -> world -> (world, [Command bid]))
  -> Game () bid tid world
singlePlayerGame initW toScene onEv =
  Game
    { gameInit = initW
    , gameWorld = \w ->
        let sc = toScene w
         in WorldScene
              { wsEpoch = sceneEpoch sc
              , wsGravity = sceneGravity sc
              , wsBackground = sceneBackground sc
              , wsFog = sceneFog sc
              , wsLights = sceneLights sc
              , wsTree = sceneTree sc
              , wsTimers = sceneTimers sc
              }
    , gameView = \_ w ->
        let sc = toScene w
         in PlayerView
              { pvCamera = sceneCamera sc
              , pvHud = sceneHud sc
              }
    , gameStep = \mev snap w ->
        case mev of
          PlayerJoined () ->
            let (w', cmds) = onEv EngineReady snap w
             in (w', map WorldCmd cmds)
          PlayerLeft () -> (w, [])
          PlayerInput () inp ->
            let ev = case inp of
                  InKeyDown k -> KeyDown k
                  InKeyUp k -> KeyUp k
                  InMouseDown b h -> MouseDown b h
                  InMouseUp b -> MouseUp b
                (w', cmds) = onEv ev snap w
             in (w', map WorldCmd cmds)
          WorldCollisionStart a b ->
            let (w', cmds) = onEv (CollisionStart a b) snap w
             in (w', map WorldCmd cmds)
          WorldCollisionEnd a b ->
            let (w', cmds) = onEv (CollisionEnd a b) snap w
             in (w', map WorldCmd cmds)
          WorldTimerFired tid ->
            let (w', cmds) = onEv (TimerFired tid) snap w
             in (w', map WorldCmd cmds)
    }
