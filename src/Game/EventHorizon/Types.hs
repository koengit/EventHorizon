module Game.EventHorizon.Types
  ( -- * Input
    Key (..)
  , MouseButton (..)

    -- * Events
  , Event (..)

    -- * Physics Snapshot
  , BodyState (..)
  , Snapshot (..)
  , emptySnapshot
  , lookupBody

    -- * One-Shot Commands & Procedural Audio
  , WaveType (..)
  , Sound (..)
  , tone
  , sweep
  , noise
  , Command (..)

    -- * Collision Pair Matching Helpers
  , matchPair
  , matchPred
  , matchEq
  , only
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Game.EventHorizon.Math

-- | Keyboard keys delivered to the game's event handler.
data Key
  = KeyChar !Char
  | KeyUpArrow
  | KeyDownArrow
  | KeyLeftArrow
  | KeyRightArrow
  | KeySpace
  | KeyEnter
  | KeyEscape
  | KeyShift
  | KeyTab
  | KeyBackspace
  | KeyOther !Text
  deriving (Eq, Ord, Show)

-- | Mouse buttons delivered on click/release.
data MouseButton
  = MouseLeft
  | MouseMiddle
  | MouseRight
  deriving (Eq, Ord, Show)

-- | Events that wake the pure game update function.
-- Parameterized by the user's body identifier type @bid@ and timer identifier type @tid@.
data Event bid tid
  = EngineReady
  | KeyDown !Key
  | KeyUp !Key
  | -- | Mouse button pressed, with an optional 3D raycast hit @(bodyId, worldPoint)@.
    MouseDown !MouseButton !(Maybe (bid, Vec3))
  | MouseUp !MouseButton
  | CollisionStart !bid !bid
  | CollisionEnd !bid !bid
  | TimerFired !tid
  deriving (Eq, Show)

-- | Live physical state of a body captured at the instant an event fires.
data BodyState = BodyState
  { bsPos :: !Vec3
  , bsRot :: !Quat
  , bsVel :: !Vec3
  , bsAngVel :: !Vec3
  }
  deriving (Eq, Show)

-- | Snapshot of simulation time and body states at the instant an event fires.
data Snapshot bid = Snapshot
  { snapTime :: !Double
  , snapBodies :: !(Map bid BodyState)
  }
  deriving (Eq, Show)

emptySnapshot :: Snapshot bid
emptySnapshot = Snapshot 0 Map.empty

lookupBody :: Ord bid => bid -> Snapshot bid -> Maybe BodyState
lookupBody bid snap = Map.lookup bid (snapBodies snap)

-- | Oscillator waveform for built-in procedural Web Audio.
data WaveType
  = WaveSine
  | WaveSquare
  | WaveSawtooth
  | WaveTriangle
  deriving (Eq, Show)

-- | Procedural sound effect synthesized directly by the browser's Web Audio API.
data Sound
  = SoundTone
      { sndWave :: !WaveType
      , sndFreqStart :: !Double
      , sndFreqEnd :: !Double
      , sndDuration :: !Double
      , sndVolume :: !Double
      }
  | SoundNoise
      { sndDuration :: !Double
      , sndVolume :: !Double
      }
  deriving (Eq, Show)

-- | Constant-pitch tone.
tone :: WaveType -> Double -> Double -> Double -> Sound
tone w freq dur vol = SoundTone w freq freq dur vol

-- | Frequency-swept tone (great for lasers, jumps, UI chimes).
sweep :: WaveType -> Double -> Double -> Double -> Double -> Sound
sweep = SoundTone

-- | Filtered white noise burst (great for explosions, impacts).
noise :: Double -> Double -> Sound
noise = SoundNoise

-- | One-shot imperative commands executed once by the physics/audio runtime
-- after an event is processed (used with 'Game.EventHorizon.playWith').
data Command bid
  = ApplyImpulse !bid !Vec3
  | ApplyTorqueImpulse !bid !Vec3
  | SetVelocity !bid !Vec3
  | SetAngularVelocity !bid !Vec3
  | Teleport !bid !Pose
  | PlaySound !Sound
  deriving (Eq, Show)

-- | Match an unordered collision pair using two partial projections.
-- Returns @Just (x, y)@ regardless of whether @(a, b)@ or @(b, a)@ matched.
matchPair
  :: (bid -> Maybe a)
  -> (bid -> Maybe b)
  -> bid
  -> bid
  -> Maybe (a, b)
matchPair f g a b =
  case (f a, g b) of
    (Just x, Just y) -> Just (x, y)
    _ -> case (f b, g a) of
      (Just x, Just y) -> Just (x, y)
      _ -> Nothing

-- | Match an unordered collision pair against two predicates.
matchPred
  :: (bid -> Bool)
  -> (bid -> Bool)
  -> bid
  -> bid
  -> Maybe (bid, bid)
matchPred p q a b
  | p a && q b = Just (a, b)
  | p b && q a = Just (b, a)
  | otherwise = Nothing

-- | Check whether an unordered collision pair matches two specific body IDs.
matchEq :: Eq bid => bid -> bid -> bid -> bid -> Bool
matchEq x y a b = (a == x && b == y) || (a == y && b == x)

-- | Helper projection for 'matchPair' that matches a single exact value.
only :: Eq a => a -> a -> Maybe ()
only expected actual
  | expected == actual = Just ()
  | otherwise = Nothing
