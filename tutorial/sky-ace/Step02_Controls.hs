{-# LANGUAGE OverloadedStrings #-}

-- | Step 2: Event-Driven Flight Controls
--
-- Adds a 'GameState' holding the currently pressed keys ('Set Key').
-- Haskell only wakes on 'KeyDown' and 'KeyUp' (never on frame ticks!) and
-- updates the plane's declarative 'FlightDrive'.
module Step02_Controls (run) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void (Void)
import Game.EventHorizon

data BodyTag
  = Player
  | Ground
  deriving (Eq, Ord, Show)

newtype GameState = GameState
  { gsHeldKeys :: Set Key
  }
  deriving (Eq, Show)

initialState :: GameState
initialState = GameState Set.empty

-- | Pure event handler: tracks held keys on discrete KeyDown / KeyUp events.
handleEvent :: Event BodyTag Void -> Snapshot BodyTag -> GameState -> GameState
handleEvent ev _ gs = case ev of
  KeyDown k -> gs {gsHeldKeys = Set.insert k (gsHeldKeys gs)}
  KeyUp k -> gs {gsHeldKeys = Set.delete k (gsHeldKeys gs)}
  _ -> gs

render :: GameState -> Scene BodyTag Void
render gs =
  defaultScene
    { sceneGravity = Vec3 0 (-18) 0
    , sceneBackground = skyBlue
    , sceneCamera =
        FollowCamera
          { camTarget = Player
          , camOffset = Vec3 0 3.0 10.5
          , camLookOffset = Vec3 0 0.4 (-14.0)
          , camLocal = True
          , camSmooth = 7.5
          , camFov = 62
          }
    , sceneTree = bodyNode groundBody <> bodyNode (playerBody gs)
    , sceneHud =
        [ HudText TopLeft "Step 2: Event-Driven Controls"
        , HudText BottomCenter "W/S or ↑/↓: Climb/Dive   ·   A/D or ←/→: Turn   ·   Shift: Boost"
        ]
    }

groundBody :: Body BodyTag
groundBody =
  staticBody
    Ground
    (at (Vec3 0 (-1) 0))
    (BoxCollider (Vec3 600 2 600))
    (colored (Color 0.24 0.52 0.24) (box (Vec3 600 2 600)))

playerBody :: GameState -> Body BodyTag
playerBody gs =
  ( dynamicBody
      Player
      (at (Vec3 0 18 52))
      (BoxCollider (Vec3 3.0 0.9 2.6))
      airplaneMesh
  )
    { bodyGravity = 0.0
    , bodyMass = 2.0
    , bodyDrive = computeDrive (gsHeldKeys gs)
    }

computeDrive :: Set Key -> Drive
computeDrive keys =
  let held k = Set.member k keys
      pitchUp = held (KeyChar 'w') || held KeyUpArrow
      pitchDown = held (KeyChar 's') || held KeyDownArrow
      turnLeft = held (KeyChar 'a') || held KeyLeftArrow
      turnRight = held (KeyChar 'd') || held KeyRightArrow
      boost = held KeyShift
      pitch = (if pitchUp then 1.3 else 0) + (if pitchDown then (-1.3) else 0)
      yaw = (if turnLeft then 1.5 else 0) + (if turnRight then (-1.5) else 0)
      speed = if boost then 39.0 else 25.0
   in FlightDrive
        { flightSpeed = speed
        , flightPitch = pitch
        , flightYaw = yaw
        , flightRoll = 0.0
        , flightAutoLevel = True
        }

airplaneMesh :: Mesh
airplaneMesh =
  group
    [ colored (Color 0.20 0.52 0.88) $ box (Vec3 0.72 0.55 2.9)
    , colored gold $ translate (Vec3 0 0 (-1.75)) $ rotate (Vec3 (-pi / 2) 0 0) $ cone 0.36 0.75
    , glow cyan 0.35 $ colored cyan $ translate (Vec3 0 0.35 (-0.25)) $ sphere 0.34
    , colored white $ translate (Vec3 0 (-0.02) 0.1) $ box (Vec3 4.4 0.12 0.95)
    , colored white $ translate (Vec3 0 0.08 1.25) $ box (Vec3 1.7 0.10 0.45)
    , colored red $ translate (Vec3 0 0.52 1.2) $ box (Vec3 0.12 0.82 0.58)
    ]

run :: Int -> IO ()
run port =
  play
    (defaultConfig "Sky Ace Tutorial — Step 2: Controls") {cfgPort = port}
    initialState
    render
    handleEvent
