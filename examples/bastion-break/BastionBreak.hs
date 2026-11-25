{-# LANGUAGE OverloadedStrings #-}

module BastionBreak
  ( SiegeBody (..)
  , SiegeTimer (..)
  , BlockMat (..)
  , AmmoKind (..)
  , CameraMode (..)
  , SiegeStatus (..)
  , BlockDef (..)
  , TntDef (..)
  , IdolDef (..)
  , FortressDef (..)
  , ShotInfo (..)
  , BlastFx (..)
  , SiegeState (..)
  , fortresses
  , initialSiege
  , initialSiegeWith
  , renderSiege
  , stepSiege
  , handleEvent
  , computeBallisticVel
  , cameraPositionFor
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Game.EventHorizon

-- | Type-safe 3D body identifiers for Bastion Break.
data SiegeBody
  = Island
  | Water
  | SeaSensor
  | -- | Invisible non-colliding pivot rotated at 60 Hz via 'VelocityDrive' in 'SmoothOrbit' mode.
    CameraPivot
  | PillarMarker !Int
  | Block !Int
  | Tnt !Int
  | Idol !Int
  | Shot !Int
  | BlastRing !Int
  deriving (Eq, Ord, Show)

-- | Type-safe timer identifiers for Bastion Break.
data SiegeTimer
  = ShotExpire !Int
  | FxExpire !Int
  | ChainTnt !Int
  | CheckOutOfAmmo
  deriving (Eq, Ord, Show)

data BlockMat
  = Stone
  | Timber
  deriving (Eq, Show)

data AmmoKind
  = AmmoIron
  | AmmoBomb
  deriving (Eq, Show)

-- | Camera control strategy:
--
-- * 'SmoothOrbit': rotates an invisible non-colliding 'CameraPivot' body at 60 Hz
--   via 'VelocityDrive' + 'FollowCamera', waking Haskell only on @KeyDown@/@KeyUp@.
-- * 'StepOrbit': uses 'FixedCamera' and increments 'ssCamAngle' in discrete steps on @KeyDown@.
data CameraMode
  = SmoothOrbit
  | StepOrbit
  deriving (Eq, Show)

data SiegeStatus
  = Playing
  | FortressCleared
  | OutOfAmmo
  | CampaignWon
  deriving (Eq, Show)

data BlockDef = BlockDef
  { blkId :: !Int
  , blkPos :: !Vec3
  , blkSize :: !Vec3
  , blkMat :: !BlockMat
  }
  deriving (Eq, Show)

data TntDef = TntDef
  { tntId :: !Int
  , tntPos :: !Vec3
  }
  deriving (Eq, Show)

data IdolDef = IdolDef
  { idolId :: !Int
  , idolPos :: !Vec3
  }
  deriving (Eq, Show)

data FortressDef = FortressDef
  { fortNumber :: !Int
  , fortName :: !Text
  , fortHint :: !Text
  , fortShots :: !Int
  , fortBombs :: !Int
  , fortBlocks :: ![BlockDef]
  , fortTnt :: ![TntDef]
  , fortIdols :: ![IdolDef]
  }
  deriving (Eq, Show)

data ShotInfo = ShotInfo
  { shotNum :: !Int
  , shotKind :: !AmmoKind
  , shotSpawnPose :: !Pose
  , shotVel :: !Vec3
  }
  deriving (Eq, Show)

data BlastFx = BlastFx
  { fxNum :: !Int
  , fxPos :: !Vec3
  , fxRadius :: !Double
  , fxColor :: !Color
  }
  deriving (Eq, Show)

data SiegeState = SiegeState
  { ssEpoch :: !Int
  , ssFortIndex :: !Int
  , ssStatus :: !SiegeStatus
  , ssScore :: !Int
  , ssCamMode :: !CameraMode
  , ssCamAngle :: !Double
  , ssHeldKeys :: !(Set Key)
  , ssAmmoKind :: !AmmoKind
  , ssShotsLeft :: !Int
  , ssBombsLeft :: !Int
  , ssActiveIdols :: !(Set Int)
  , ssActiveTnt :: !(Set Int)
  , ssPendingChainTnt :: !(Set Int)
  , ssShots :: !(Map Int ShotInfo)
  , ssFx :: !(Map Int BlastFx)
  , ssNextId :: !Int
  , ssLastShotTime :: !Double
  , ssSimTime :: !Double
  }
  deriving (Eq, Show)

siegeGravityY :: Double
siegeGravityY = -24.0

-- | Three handcrafted destructible 3D fortresses with mathematically exact,
-- non-overlapping block stacks resting on platform top y = 0.0.
fortresses :: [FortressDef]
fortresses = [fortress1, fortress2, fortress3]

-- | Fortress 1: The Stone Arch
-- A central stone arch with a TNT barrel underneath and a timber belfry above.
fortress1 :: FortressDef
fortress1 =
  FortressDef
    { fortNumber = 1
    , fortName = "The Stone Arch"
    , fortHint = "Click the red TNT barrel under the arch to collapse the entire monument!"
    , fortShots = 6
    , fortBombs = 2
    , fortBlocks =
        [ -- Ground-level stone pillars (height 4.0 -> center y = 2.0)
          BlockDef 1 (Vec3 (-3.2) 2.0 0) (Vec3 1.4 4.0 2.4) Stone
        , BlockDef 2 (Vec3 3.2 2.0 0) (Vec3 1.4 4.0 2.4) Stone
        , -- Main stone lintel (y in [4.0, 5.0] -> center y = 4.5)
          BlockDef 3 (Vec3 0 4.5 0) (Vec3 8.8 1.0 2.6) Stone
        , -- Upper timber belfry pillars (y in [5.0, 8.2] -> center y = 6.6)
          BlockDef 4 (Vec3 (-2.0) 6.6 0) (Vec3 0.9 3.2 1.6) Timber
        , BlockDef 5 (Vec3 2.0 6.6 0) (Vec3 0.9 3.2 1.6) Timber
        , -- Roof beam (y in [8.2, 9.0] -> center y = 8.6)
          BlockDef 6 (Vec3 0 8.6 0) (Vec3 5.6 0.8 1.8) Timber
        , -- Decorative side buttresses (y in [0.0, 2.8] -> center y = 1.4)
          BlockDef 7 (Vec3 (-5.2) 1.4 0) (Vec3 1.2 2.8 1.8) Stone
        , BlockDef 8 (Vec3 5.2 1.4 0) (Vec3 1.2 2.8 1.8) Stone
        ]
    , fortTnt =
        [ TntDef 1 (Vec3 0 0.75 0)
        ]
    , fortIdols =
        [ IdolDef 1 (Vec3 0 5.85 0)
        , IdolDef 2 (Vec3 0 9.85 0)
        ]
    }

-- | Fortress 2: Twin Watchtowers
-- Two multi-story towers linked by a high wooden skybridge and dual TNT barrels.
fortress2 :: FortressDef
fortress2 =
  FortressDef
    { fortNumber = 2
    , fortName = "Twin Watchtowers"
    , fortHint = "Hit the TNT barrels or orbit with [A]/[D] to target the timber supports!"
    , fortShots = 7
    , fortBombs = 2
    , fortBlocks =
        [ -- West Tower base pillars (x around -5.5, y in [0, 3.6])
          BlockDef 1 (Vec3 (-6.9) 1.8 (-1.1)) (Vec3 1.0 3.6 1.0) Stone
        , BlockDef 2 (Vec3 (-4.1) 1.8 (-1.1)) (Vec3 1.0 3.6 1.0) Stone
        , BlockDef 3 (Vec3 (-6.9) 1.8 1.1) (Vec3 1.0 3.6 1.0) Stone
        , BlockDef 4 (Vec3 (-4.1) 1.8 1.1) (Vec3 1.0 3.6 1.0) Stone
        , -- West Tower floor 2 (y in [3.6, 4.4])
          BlockDef 5 (Vec3 (-5.5) 4.0 0) (Vec3 4.2 0.8 3.6) Timber
        , -- West Tower upper pillars (y in [4.4, 7.6])
          BlockDef 6 (Vec3 (-6.7) 6.0 0) (Vec3 0.9 3.2 2.4) Timber
        , BlockDef 7 (Vec3 (-4.3) 6.0 0) (Vec3 0.9 3.2 2.4) Timber
        , -- West Tower roof (y in [7.6, 8.4])
          BlockDef 8 (Vec3 (-5.5) 8.0 0) (Vec3 3.8 0.8 2.8) Stone
        , -- East Tower base pillars (x around +5.5, y in [0, 3.6])
          BlockDef 9 (Vec3 4.1 1.8 (-1.1)) (Vec3 1.0 3.6 1.0) Stone
        , BlockDef 10 (Vec3 6.9 1.8 (-1.1)) (Vec3 1.0 3.6 1.0) Stone
        , BlockDef 11 (Vec3 4.1 1.8 1.1) (Vec3 1.0 3.6 1.0) Stone
        , BlockDef 12 (Vec3 6.9 1.8 1.1) (Vec3 1.0 3.6 1.0) Stone
        , -- East Tower floor 2 (y in [3.6, 4.4])
          BlockDef 13 (Vec3 5.5 4.0 0) (Vec3 4.2 0.8 3.6) Timber
        , -- East Tower upper pillars (y in [4.4, 7.6])
          BlockDef 14 (Vec3 4.3 6.0 0) (Vec3 0.9 3.2 2.4) Timber
        , BlockDef 15 (Vec3 6.7 6.0 0) (Vec3 0.9 3.2 2.4) Timber
        , -- East Tower roof (y in [7.6, 8.4])
          BlockDef 16 (Vec3 5.5 8.0 0) (Vec3 3.8 0.8 2.8) Stone
        , -- Central skybridge spanning between both towers (y in [4.4, 5.1])
          BlockDef 17 (Vec3 0 4.75 0) (Vec3 7.6 0.7 2.2) Timber
        , -- Central lower vault pedestal (y in [0, 1.4])
          BlockDef 18 (Vec3 0 0.7 0) (Vec3 2.4 1.4 2.4) Stone
        ]
    , fortTnt =
        [ TntDef 1 (Vec3 (-5.5) 0.75 0)
        , TntDef 2 (Vec3 0 5.85 0)
        ]
    , fortIdols =
        [ IdolDef 1 (Vec3 (-5.5) 9.25 0)
        , IdolDef 2 (Vec3 5.5 9.25 0)
        , IdolDef 3 (Vec3 0 2.25 0)
        ]
    }

-- | Fortress 3: The Citadel
-- Outer stone curtain wall shielding a 3-tier inner keep with a TNT chain reaction.
fortress3 :: FortressDef
fortress3 =
  FortressDef
    { fortNumber = 3
    , fortName = "The Citadel"
    , fortHint = "Breach the curtain wall or switch to Bomb Shot [Q] to ignite the citadel's TNT chain!"
    , fortShots = 8
    , fortBombs = 3
    , fortBlocks =
        [ -- Front curtain wall (z = 4.5, y in [0, 3.2])
          BlockDef 1 (Vec3 (-4.0) 1.6 4.5) (Vec3 2.6 3.2 1.1) Stone
        , BlockDef 2 (Vec3 0 1.6 4.5) (Vec3 2.8 3.2 1.1) Timber
        , BlockDef 3 (Vec3 4.0 1.6 4.5) (Vec3 2.6 3.2 1.1) Stone
        , -- Curtain wall cap beam (y in [3.2, 3.9])
          BlockDef 4 (Vec3 (-2.0) 3.55 4.5) (Vec3 4.4 0.7 1.2) Stone
        , BlockDef 5 (Vec3 2.0 3.55 4.5) (Vec3 4.4 0.7 1.2) Stone
        , -- Inner keep 4 corner stone columns (z in [-2.4, 1.4], x in [-3.4, 3.4], y in [0, 4.0])
          BlockDef 6 (Vec3 (-3.4) 2.0 (-2.4)) (Vec3 1.3 4.0 1.3) Stone
        , BlockDef 7 (Vec3 3.4 2.0 (-2.4)) (Vec3 1.3 4.0 1.3) Stone
        , BlockDef 8 (Vec3 (-3.4) 2.0 1.4) (Vec3 1.3 4.0 1.3) Stone
        , BlockDef 9 (Vec3 3.4 2.0 1.4) (Vec3 1.3 4.0 1.3) Stone
        , -- Keep deck slab (y in [4.0, 4.9])
          BlockDef 10 (Vec3 0 4.45 (-0.5)) (Vec3 8.6 0.9 5.4) Stone
        , -- Keep second tier timber columns (y in [4.9, 8.1])
          BlockDef 11 (Vec3 (-2.2) 6.5 (-1.6)) (Vec3 1.0 3.2 1.0) Timber
        , BlockDef 12 (Vec3 2.2 6.5 (-1.6)) (Vec3 1.0 3.2 1.0) Timber
        , BlockDef 13 (Vec3 (-2.2) 6.5 0.6) (Vec3 1.0 3.2 1.0) Timber
        , BlockDef 14 (Vec3 2.2 6.5 0.6) (Vec3 1.0 3.2 1.0) Timber
        , -- Crown roof slab (y in [8.1, 8.9])
          BlockDef 15 (Vec3 0 8.5 (-0.5)) (Vec3 6.0 0.8 3.6) Timber
        , -- Flanking flank pillars holding side idols (y in [0, 3.4])
          BlockDef 16 (Vec3 (-6.8) 1.7 (-0.5)) (Vec3 1.8 3.4 1.8) Stone
        , BlockDef 17 (Vec3 6.8 1.7 (-0.5)) (Vec3 1.8 3.4 1.8) Stone
        ]
    , fortTnt =
        [ TntDef 1 (Vec3 0 0.75 (-0.5))
        , TntDef 2 (Vec3 (-3.2) 5.65 (-0.5))
        , TntDef 3 (Vec3 3.2 5.65 (-0.5))
        ]
    , fortIdols =
        [ IdolDef 1 (Vec3 0 9.75 (-0.5))
        , IdolDef 2 (Vec3 0 5.75 (-0.5))
        , IdolDef 3 (Vec3 (-6.8) 4.25 (-0.5))
        , IdolDef 4 (Vec3 6.8 4.25 (-0.5))
        ]
    }

-- | Shared static island, stepped stone plinth (CompoundCollider), water, and fall sensor.
-- Held as a top-level CAF 'SceneTree' so EventHorizon's diff skips it in O(1) on every event!
staticIslandBodies :: [Body SiegeBody]
staticIslandBodies =
  [ islandBody
  , waterBody
  , seaSensorBody
  ]
    <> [ cornerMarker i x z
       | (i, (x, z)) <- zip [1 ..] [(-9.5, -7.5), (9.5, -7.5), (-9.5, 7.5), (9.5, 7.5)]
       ]

staticIslandTree :: SceneTree SiegeBody
staticIslandTree = bodies staticIslandBodies

fortressBlockTrees :: [SceneTree SiegeBody]
fortressBlockTrees = [bodies (map blockBody (fortBlocks f)) | f <- fortresses]

islandBody :: Body SiegeBody
islandBody =
  let -- Stepped island platform using a CompoundCollider (top deck + lower rocky skirt)
      col =
        CompoundCollider
          [ (at (Vec3 0 (-1.0) 0), BoxCollider (Vec3 22 2.0 18))
          , (at (Vec3 0 (-3.0) 0), BoxCollider (Vec3 26 2.0 22))
          ]
      mesh =
        group
          [ colored (Color 0.34 0.44 0.30) $ translate (Vec3 0 (-0.1) 0) $ box (Vec3 22 0.2 18)
          , colored (Color 0.38 0.36 0.34) $ translate (Vec3 0 (-1.1) 0) $ box (Vec3 21.8 1.8 17.8)
          , colored (Color 0.28 0.27 0.26) $ translate (Vec3 0 (-3.0) 0) $ box (Vec3 26 2.0 22)
          ]
   in (staticBody Island (at vZero) col mesh)
        { bodyFriction = 0.85
        , bodyRestitution = 0.05
        }

waterBody :: Body SiegeBody
waterBody =
  let waterMat =
        defaultMaterial
          { matColor = Just (Color 0.10 0.36 0.62)
          , matRoughness = 0.22
          , matMetalness = 0.25
          , matOpacity = 0.88
          , matCastShadow = False
          , matReceiveShadow = True
          }
   in staticBody
        Water
        (at (Vec3 0 (-5.2) 0))
        NoCollider
        (withMaterial waterMat (box (Vec3 220 0.4 220)))

seaSensorBody :: Body SiegeBody
seaSensorBody =
  sensorBody
    SeaSensor
    (at (Vec3 0 (-5.0) 0))
    (BoxCollider (Vec3 180 2.0 180))
    emptyMesh

cornerMarker :: Int -> Double -> Double -> Body SiegeBody
cornerMarker i x z =
  let mesh =
        group
          [ colored darkGray $ translate (Vec3 0 0.55 0) $ cylinder 0.35 0.45 1.1
          , glow orange 1.5 $ colored orange $ translate (Vec3 0 1.25 0) $ sphere 0.22
          ]
   in staticBody (PillarMarker i) (at (Vec3 x 0 z)) (CylinderCollider 0.45 1.1) mesh

initialSiege :: SiegeState
initialSiege = initialSiegeWith SmoothOrbit

initialSiegeWith :: CameraMode -> SiegeState
initialSiegeWith mode = startFortress mode 0 0 0

startFortress :: CameraMode -> Int -> Int -> Int -> SiegeState
startFortress mode epoch idx score =
  let safeIdx = max 0 (min (length fortresses - 1) idx)
      f = fortresses !! safeIdx
   in SiegeState
        { ssEpoch = epoch
        , ssFortIndex = safeIdx
        , ssStatus = Playing
        , ssScore = score
        , ssCamMode = mode
        , ssCamAngle = 0.0
        , ssHeldKeys = Set.empty
        , ssAmmoKind = AmmoIron
        , ssShotsLeft = fortShots f
        , ssBombsLeft = fortBombs f
        , ssActiveIdols = Set.fromList (map idolId (fortIdols f))
        , ssActiveTnt = Set.fromList (map tntId (fortTnt f))
        , ssPendingChainTnt = Set.empty
        , ssShots = Map.empty
        , ssFx = Map.empty
        , ssNextId = 1
        , ssLastShotTime = -10.0
        , ssSimTime = 0.0
        }

currentFortress :: SiegeState -> FortressDef
currentFortress ss = fortresses !! ssFortIndex ss

camOrbitOffset :: Vec3
camOrbitOffset = Vec3 0 11.5 25.0

camLookTarget :: Vec3
camLookTarget = Vec3 0 4.2 0

cameraPositionFor :: Double -> Vec3
cameraPositionFor angle =
  quatFromEuler (Vec3 0 angle 0) #> camOrbitOffset

liveCameraPosition :: Snapshot SiegeBody -> SiegeState -> Vec3
liveCameraPosition snap ss = case ssCamMode ss of
  StepOrbit -> cameraPositionFor (ssCamAngle ss)
  SmoothOrbit ->
    case lookupBody CameraPivot snap of
      Just st -> bsPos st + (bsRot st #> camOrbitOffset)
      Nothing -> cameraPositionFor (ssCamAngle ss)

liveCameraAngle :: Snapshot SiegeBody -> SiegeState -> Double
liveCameraAngle snap ss = case ssCamMode ss of
  StepOrbit -> ssCamAngle ss
  SmoothOrbit ->
    case lookupBody CameraPivot snap of
      Just st ->
        let Vec3 cx _ cz = bsRot st #> Vec3 0 0 1
         in atan2 cx cz
      Nothing -> ssCamAngle ss

cannonMuzzleFrom :: Vec3 -> Vec3
cannonMuzzleFrom cam =
  let toTarget = vNorm (Vec3 0 4.0 0 - cam)
   in cam + 2.8 *^ toTarget + Vec3 0 (-1.6) 0

-- | Compute the exact ballistic launch velocity to arc from @start@ to @target@
-- under gravity @siegeGravityY@ at horizontal cruise speed @speed@.
computeBallisticVel :: Vec3 -> Vec3 -> Double -> Vec3
computeBallisticVel start target speed =
  let Vec3 dx dy dz = target - start
      horiz = sqrt (dx * dx + dz * dz)
      flightTime = max 0.25 (min 1.35 (horiz / max 5.0 speed))
      vx = dx / flightTime
      vz = dz / flightTime
      vy = (dy - 0.5 * siegeGravityY * flightTime * flightTime) / flightTime
   in Vec3 vx vy vz

-- | Pure event handler returning updated state plus radial impulses and audio commands.
stepSiege
  :: Event SiegeBody SiegeTimer
  -> Snapshot SiegeBody
  -> SiegeState
  -> (SiegeState, [Command SiegeBody])
stepSiege ev snap ss0 =
  let ss = ss0 {ssSimTime = snapTime snap}
   in case ev of
        EngineReady -> (ss, [])
        KeyDown (KeyChar 'r') ->
          let next =
                if ssStatus ss == CampaignWon
                  then startFortress (ssCamMode ss) (ssEpoch ss + 1) 0 0
                  else startFortress (ssCamMode ss) (ssEpoch ss + 1) (ssFortIndex ss) (ssScore ss)
           in (next, [])
        KeyDown (KeyChar '1') -> (startFortress (ssCamMode ss) (ssEpoch ss + 1) 0 (ssScore ss), [])
        KeyDown (KeyChar '2') -> (startFortress (ssCamMode ss) (ssEpoch ss + 1) 1 (ssScore ss), [])
        KeyDown (KeyChar '3') -> (startFortress (ssCamMode ss) (ssEpoch ss + 1) 2 (ssScore ss), [])
        KeyDown (KeyChar 'c') ->
          let nextMode = case ssCamMode ss of
                SmoothOrbit -> StepOrbit
                StepOrbit -> SmoothOrbit
              syncedAngle = liveCameraAngle snap ss
           in ( ss {ssCamMode = nextMode, ssCamAngle = syncedAngle, ssHeldKeys = Set.empty}
              , [PlaySound (tone WaveSine 540 0.06 0.12)]
              )
        KeyDown (KeyChar 'n') -> advanceIfCleared ss
        KeyDown KeyEnter -> advanceIfCleared ss
        KeyDown k | isOrbitLeftKey k ->
          case ssCamMode ss of
            StepOrbit -> (ss {ssCamAngle = ssCamAngle ss - (pi / 8)}, [])
            SmoothOrbit -> (ss {ssHeldKeys = Set.insert k (ssHeldKeys ss)}, [])
        KeyDown k | isOrbitRightKey k ->
          case ssCamMode ss of
            StepOrbit -> (ss {ssCamAngle = ssCamAngle ss + (pi / 8)}, [])
            SmoothOrbit -> (ss {ssHeldKeys = Set.insert k (ssHeldKeys ss)}, [])
        KeyDown (KeyChar 'q') -> toggleAmmo ss
        KeyDown KeyTab -> toggleAmmo ss
        MouseDown MouseRight _ -> toggleAmmo ss
        MouseDown MouseLeft (Just (hitBid, hitPt)) ->
          if ssStatus ss /= Playing || hitBid == Water
            then (ss, [])
            else fireAt snap hitPt ss
        MouseDown _ _ -> (ss, [])
        MouseUp _ -> (ss, [])
        KeyDown KeySpace ->
          case ssStatus ss of
            FortressCleared -> advanceIfCleared ss
            Playing ->
              let aimPt = pickDefaultTarget snap ss
               in fireAt snap aimPt ss
            _ -> (ss, [])
        KeyDown _ -> (ss, [])
        KeyUp k ->
          (ss {ssHeldKeys = Set.delete k (ssHeldKeys ss)}, [])
        TimerFired tid -> handleTimer tid snap ss
        CollisionStart a b -> handleCollision a b snap ss
        CollisionEnd _ _ -> (ss, [])

isOrbitLeftKey :: Key -> Bool
isOrbitLeftKey (KeyChar 'a') = True
isOrbitLeftKey KeyLeftArrow = True
isOrbitLeftKey _ = False

isOrbitRightKey :: Key -> Bool
isOrbitRightKey (KeyChar 'd') = True
isOrbitRightKey KeyRightArrow = True
isOrbitRightKey _ = False

handleEvent :: Event SiegeBody SiegeTimer -> Snapshot SiegeBody -> SiegeState -> SiegeState
handleEvent ev snap ss = fst (stepSiege ev snap ss)

advanceIfCleared :: SiegeState -> (SiegeState, [Command SiegeBody])
advanceIfCleared ss
  | ssStatus ss == FortressCleared && ssFortIndex ss + 1 < length fortresses =
      (startFortress (ssCamMode ss) (ssEpoch ss + 1) (ssFortIndex ss + 1) (ssScore ss), [])
  | otherwise = (ss, [])

toggleAmmo :: SiegeState -> (SiegeState, [Command SiegeBody])
toggleAmmo ss =
  let nextKind = case ssAmmoKind ss of
        AmmoIron -> if ssBombsLeft ss > 0 then AmmoBomb else AmmoIron
        AmmoBomb -> AmmoIron
      sfx = [PlaySound (tone WaveSine 620 0.06 0.12) | nextKind /= ssAmmoKind ss]
   in (ss {ssAmmoKind = nextKind}, sfx)

pickDefaultTarget :: Snapshot SiegeBody -> SiegeState -> Vec3
pickDefaultTarget snap ss =
  case Set.toList (ssActiveTnt ss) of
    (tid : _) -> maybe (Vec3 0 3.0 0) bsPos (lookupBody (Tnt tid) snap)
    [] ->
      case Set.toList (ssActiveIdols ss) of
        (iid : _) -> maybe (Vec3 0 4.0 0) bsPos (lookupBody (Idol iid) snap)
        [] -> Vec3 0 4.0 0

fireAt :: Snapshot SiegeBody -> Vec3 -> SiegeState -> (SiegeState, [Command SiegeBody])
fireAt snap targetPt ss
  | ssStatus ss /= Playing = (ss, [])
  | ssShotsLeft ss <= 0 = (ss, [])
  | ssSimTime ss - ssLastShotTime ss < 0.25 = (ss, [])
  | otherwise =
      let useBomb = ssAmmoKind ss == AmmoBomb && ssBombsLeft ss > 0
          kind = if useBomb then AmmoBomb else AmmoIron
          speed = case kind of
            AmmoIron -> 42.0
            AmmoBomb -> 34.0
          muzzle = cannonMuzzleFrom (liveCameraPosition snap ss)
          vel = computeBallisticVel muzzle targetPt speed
          nid = ssNextId ss
          shot = ShotInfo nid kind (at muzzle) vel
          nextBombs = if useBomb then ssBombsLeft ss - 1 else ssBombsLeft ss
          nextAmmo = if nextBombs <= 0 then AmmoIron else ssAmmoKind ss
          cannonSfx =
            case kind of
              AmmoIron ->
                [ PlaySound (noise 0.18 0.28)
                , PlaySound (sweep WaveTriangle 180 55 0.22 0.26)
                ]
              AmmoBomb ->
                [ PlaySound (noise 0.16 0.22)
                , PlaySound (sweep WaveSawtooth 320 90 0.24 0.22)
                ]
       in ( ss
              { ssShotsLeft = ssShotsLeft ss - 1
              , ssBombsLeft = nextBombs
              , ssAmmoKind = nextAmmo
              , ssShots = Map.insert nid shot (ssShots ss)
              , ssNextId = nid + 1
              , ssLastShotTime = ssSimTime ss
              }
          , cannonSfx
          )

-- | Compute radial physics impulses for every dynamic body within blast radius.
buildBlastImpulses :: Vec3 -> Double -> Double -> Snapshot SiegeBody -> [Command SiegeBody]
buildBlastImpulses center radius strength snap =
  catMaybes
    [ case bid of
        Block _ -> impulseFor bid (bsPos st)
        Tnt _ -> impulseFor bid (bsPos st)
        Idol _ -> impulseFor bid (bsPos st)
        _ -> Nothing
    | (bid, st) <- Map.toList (snapBodies snap)
    ]
  where
    impulseFor bid pos =
      let diff = pos - center
          dist = vLen diff
       in if dist > radius
            then Nothing
            else
              let falloff = 1.0 - (dist / radius)
                  dir = if dist < 0.05 then Vec3 0 1 0 else vNorm (diff + Vec3 0 2.0 0)
                  mag = strength * (0.35 + 0.65 * falloff)
               in Just (ApplyImpulse bid (mag *^ dir))

detonateAt
  :: Vec3
  -> Double
  -> Double
  -> Color
  -> Snapshot SiegeBody
  -> SiegeState
  -> (SiegeState, [Command SiegeBody])
detonateAt center radius strength col snap ss =
  let impulses = buildBlastImpulses center radius strength snap
      -- Queue chain detonations for any surviving TNT barrels caught inside the blast
      chainIds =
        [ tid
        | tid <- Set.toList (ssActiveTnt ss)
        , Just st <- [lookupBody (Tnt tid) snap]
        , vDist (bsPos st) center <= radius * 0.78
        ]
      -- Directly shatter any idol caught right next to the epicenter
      shatteredIdols =
        [ iid
        | iid <- Set.toList (ssActiveIdols ss)
        , Just st <- [lookupBody (Idol iid) snap]
        , vDist (bsPos st) center <= radius * 0.42
        ]
      nid = ssNextId ss
      fx = BlastFx nid center (radius * 0.45) col
      ss1 =
        ss
          { ssFx = Map.insert nid fx (ssFx ss)
          , ssNextId = nid + 1
          , ssPendingChainTnt = Set.union (ssPendingChainTnt ss) (Set.fromList chainIds)
          }
      (ss2, idolCmds) = foldl captureOne (ss1, []) shatteredIdols
      blastSfx =
        [ PlaySound (noise 0.42 0.42)
        , PlaySound (sweep WaveSawtooth 140 36 0.45 0.30)
        ]
   in (ss2, impulses <> blastSfx <> idolCmds)
  where
    captureOne (stAcc, cmdAcc) iid =
      let (st', cs) = captureIdol iid stAcc
       in (st', cmdAcc <> cs)

captureIdol :: Int -> SiegeState -> (SiegeState, [Command SiegeBody])
captureIdol iid ss
  | not (Set.member iid (ssActiveIdols ss)) = (ss, [])
  | otherwise =
      let remaining = Set.delete iid (ssActiveIdols ss)
          bonus = if Set.null remaining then ssShotsLeft ss * 150 else 0
          nextScore = ssScore ss + 250 + bonus
          nextStatus
            | not (Set.null remaining) = ssStatus ss
            | ssFortIndex ss + 1 < length fortresses = FortressCleared
            | otherwise = CampaignWon
          sfx =
            if Set.null remaining
              then [PlaySound (sweep WaveTriangle 440 990 0.42 0.26)]
              else [PlaySound (sweep WaveSine 520 840 0.20 0.22)]
       in ( ss
              { ssActiveIdols = remaining
              , ssScore = nextScore
              , ssStatus = nextStatus
              }
          , sfx
          )

handleTimer
  :: SiegeTimer
  -> Snapshot SiegeBody
  -> SiegeState
  -> (SiegeState, [Command SiegeBody])
handleTimer tid snap ss = case tid of
  ShotExpire n ->
    (ss {ssShots = Map.delete n (ssShots ss)}, [])
  FxExpire n ->
    (ss {ssFx = Map.delete n (ssFx ss)}, [])
  ChainTnt tId ->
    if Set.member tId (ssActiveTnt ss)
      then
        let pos = maybe (Vec3 0 2 0) bsPos (lookupBody (Tnt tId) snap)
            ss1 =
              ss
                { ssActiveTnt = Set.delete tId (ssActiveTnt ss)
                , ssPendingChainTnt = Set.delete tId (ssPendingChainTnt ss)
                , ssScore = ssScore ss + 100
                }
         in detonateAt pos 9.5 68.0 orange snap ss1
      else (ss {ssPendingChainTnt = Set.delete tId (ssPendingChainTnt ss)}, [])
  CheckOutOfAmmo ->
    if ssStatus ss == Playing
      && ssShotsLeft ss == 0
      && Map.null (ssShots ss)
      && Set.null (ssPendingChainTnt ss)
      && not (Set.null (ssActiveIdols ss))
      then (ss {ssStatus = OutOfAmmo}, [PlaySound (sweep WaveSawtooth 260 110 0.40 0.22)])
      else (ss, [])

asShot :: SiegeBody -> Maybe Int
asShot (Shot n) = Just n
asShot _ = Nothing

asTnt :: SiegeBody -> Maybe Int
asTnt (Tnt n) = Just n
asTnt _ = Nothing

asIdol :: SiegeBody -> Maybe Int
asIdol (Idol n) = Just n
asIdol _ = Nothing

isSolidTarget :: SiegeBody -> Bool
isSolidTarget b = case b of
  Block _ -> True
  Tnt _ -> True
  Idol _ -> True
  Island -> True
  PillarMarker _ -> True
  _ -> False

handleCollision
  :: SiegeBody
  -> SiegeBody
  -> Snapshot SiegeBody
  -> SiegeState
  -> (SiegeState, [Command SiegeBody])
handleCollision a b snap ss
  -- Any idol falling into the sea sensor is captured!
  | Just (iid, ()) <- matchPair asIdol (only SeaSensor) a b =
      captureIdol iid ss
  -- Shot hits TNT barrel -> massive explosion + radial impulses!
  | Just (sNum, tId) <- matchPair asShot asTnt a b =
      if Set.member tId (ssActiveTnt ss)
        then
          let pos = maybe (Vec3 0 2 0) bsPos (lookupBody (Tnt tId) snap)
              ss1 =
                ss
                  { ssActiveTnt = Set.delete tId (ssActiveTnt ss)
                  , ssPendingChainTnt = Set.delete tId (ssPendingChainTnt ss)
                  , ssShots = Map.delete sNum (ssShots ss)
                  , ssScore = ssScore ss + 100
                  }
           in detonateAt pos 9.5 72.0 orange snap ss1
        else (ss, [])
  -- Direct shot hit on a golden idol shatters & captures it!
  | Just (sNum, iid) <- matchPair asShot asIdol a b =
      let pos = maybe (Vec3 0 3 0) bsPos (lookupBody (Idol iid) snap)
          (ss1, idolCmds) = captureIdol iid ss
          nid = ssNextId ss1
          fx = BlastFx nid pos 2.2 gold
          ss2 = ss1 {ssFx = Map.insert nid fx (ssFx ss1), ssNextId = nid + 1}
       in case Map.lookup sNum (ssShots ss) of
            Just sh | shotKind sh == AmmoBomb ->
              let (ss3, blastCmds) =
                    detonateAt pos 7.5 52.0 red snap (ss2 {ssShots = Map.delete sNum (ssShots ss2)})
               in (ss3, idolCmds <> blastCmds)
            _ -> (ss2, idolCmds)
  -- Bomb shot hitting any solid surface detonates immediately!
  | Just (sNum, _) <- matchPair asShot (\x -> if isSolidTarget x then Just x else Nothing) a b =
      case Map.lookup sNum (ssShots ss) of
        Just sh | shotKind sh == AmmoBomb ->
          let pos = maybe (posePos (shotSpawnPose sh)) bsPos (lookupBody (Shot sNum) snap)
              ss1 = ss {ssShots = Map.delete sNum (ssShots ss)}
           in detonateAt pos 7.5 55.0 red snap ss1
        Just _ ->
          (ss, [PlaySound (noise 0.06 0.10)])
        Nothing -> (ss, [])
  | otherwise = (ss, [])

shotNode :: Int -> ShotInfo -> SceneTree SiegeBody
shotNode _ = bodyNode . shotBody

fxNode :: Int -> BlastFx -> SceneTree SiegeBody
fxNode _ = bodyNode . fxBody

-- | Pure render function: produces declarative 3D Scene for Bastion Break.
renderSiege :: SiegeState -> Scene SiegeBody SiegeTimer
renderSiege ss =
  let fort = currentFortress ss
      cam = case ssCamMode ss of
        StepOrbit ->
          FixedCamera
            { camPos = cameraPositionFor (ssCamAngle ss)
            , camLookAt = camLookTarget
            , camFov = 54
            }
        SmoothOrbit ->
          FollowCamera
            { camTarget = CameraPivot
            , camOffset = camOrbitOffset
            , camLookOffset = camLookTarget
            , camLocal = True
            , camSmooth = 14.0
            , camFov = 54
            }
      pivotTree =
        if ssCamMode ss == SmoothOrbit
          then bodyNode (cameraPivotBody ss)
          else mempty
   in defaultScene
        { sceneEpoch = ssEpoch ss
        , sceneGravity = Vec3 0 siegeGravityY 0
        , sceneBackground = Color 0.56 0.78 0.95
        , sceneFog = Just (Color 0.62 0.80 0.94, 48, 175)
        , sceneCamera = cam
        , sceneLights =
            [ HemisphereLight (Color 0.88 0.94 1.0) (Color 0.30 0.34 0.28) 0.75
            , DirectionalLight (Color 1.0 0.95 0.84) 1.35 (Vec3 22 38 26)
            ]
        , sceneTree =
            staticIslandTree
              <> pivotTree
              <> (fortressBlockTrees !! ssFortIndex ss)
              <> bodies [tntBody t | t <- fortTnt fort, Set.member (tntId t) (ssActiveTnt ss)]
              <> bodies [idolBody i | i <- fortIdols fort, Set.member (idolId i) (ssActiveIdols ss)]
              <> mapTree shotNode (ssShots ss)
              <> mapTree fxNode (ssFx ss)
        , sceneTimers = activeTimers ss
        , sceneHud = buildHud ss fort
        }

-- | Invisible, non-colliding pivot rotated at 60 Hz inside Rapier3D while A/D is held.
cameraPivotBody :: SiegeState -> Body SiegeBody
cameraPivotBody ss =
  let keys = ssHeldKeys ss
      leftHeld = Set.member (KeyChar 'a') keys || Set.member KeyLeftArrow keys
      rightHeld = Set.member (KeyChar 'd') keys || Set.member KeyRightArrow keys
      yawRate = (if rightHeld then 1.25 else 0.0) + (if leftHeld then (-1.25) else 0.0)
      base =
        dynamicBody
          CameraPivot
          (atRot vZero (quatFromEuler (Vec3 0 (ssCamAngle ss) 0)))
          (SphereCollider 0.1)
          emptyMesh
   in base
        { bodyGravity = 0.0
        , bodyFilter = CollisionFilter 0 0
        , bodyEvents = False
        , bodyDrive = VelocityDrive vZero (Vec3 0 yawRate 0)
        }

blockBody :: BlockDef -> Body SiegeBody
blockBody blk =
  let (col, mass, fric, rough) = case blkMat blk of
        Stone -> (Color 0.58 0.60 0.64, 3.2, 0.82, 0.80)
        Timber -> (Color 0.66 0.44 0.22, 1.4, 0.70, 0.68)
      mat =
        defaultMaterial
          { matColor = Just col
          , matRoughness = rough
          , matMetalness = 0.05
          }
      base =
        dynamicBody
          (Block (blkId blk))
          (at (blkPos blk))
          (BoxCollider (blkSize blk))
          (withMaterial mat (box (blkSize blk)))
   in base
        { bodyMass = mass
        , bodyFriction = fric
        , bodyRestitution = 0.04
        , bodyDamping = (0.15, 0.25)
        , -- Blocks don't need block-vs-block events in Haskell; Shots/TNT/Idols still report hits!
          bodyEvents = False
        }

tntBody :: TntDef -> Body SiegeBody
tntBody t =
  let mesh =
        group
          [ colored red $ cylinder 0.65 0.65 1.45
          , colored darkGray $ translate (Vec3 0 0.45 0) $ cylinder 0.68 0.68 0.14
          , colored darkGray $ translate (Vec3 0 (-0.45) 0) $ cylinder 0.68 0.68 0.14
          , glow yellow 1.4 $ colored yellow $ translate (Vec3 0 0.78 0) $ sphere 0.18
          ]
      base =
        dynamicBody
          (Tnt (tntId t))
          (at (tntPos t))
          (CylinderCollider 0.66 1.48)
          mesh
   in base
        { bodyMass = 1.6
        , bodyFriction = 0.75
        , bodyDamping = (0.2, 0.3)
        }

idolBody :: IdolDef -> Body SiegeBody
idolBody i =
  let goldMat =
        defaultMaterial
          { matColor = Just gold
          , matEmissive = Just (gold, 0.45)
          , matRoughness = 0.22
          , matMetalness = 0.82
          }
      mesh =
        group
          [ withMaterial goldMat $ cylinder 0.42 0.58 0.55
          , withMaterial goldMat $ translate (Vec3 0 0.58 0) $ sphere 0.44
          , glow yellow 1.6 $
              colored yellow $
                translate (Vec3 0 0.58 0) $
                  rotate (Vec3 (pi / 2) 0 0) $
                    torus 0.62 0.07
          ]
      base =
        dynamicBody
          (Idol (idolId i))
          (at (idolPos i))
          (CylinderCollider 0.55 1.55)
          mesh
   in base
        { bodyMass = 1.2
        , bodyFriction = 0.65
        , bodyDamping = (0.15, 0.25)
        }

shotBody :: ShotInfo -> Body SiegeBody
shotBody sh =
  let (radius, mass, mat) = case shotKind sh of
        AmmoIron ->
          ( 0.58
          , 18.0
          , defaultMaterial
              { matColor = Just (Color 0.18 0.20 0.24)
              , matRoughness = 0.28
              , matMetalness = 0.88
              }
          )
        AmmoBomb ->
          ( 0.62
          , 9.0
          , defaultMaterial
              { matColor = Just red
              , matEmissive = Just (orange, 1.8)
              , matRoughness = 0.35
              , matMetalness = 0.25
              }
          )
      base =
        dynamicBody
          (Shot (shotNum sh))
          (shotSpawnPose sh)
          (SphereCollider radius)
          (withMaterial mat (sphere radius))
   in base
        { bodyInitialVel = shotVel sh
        , bodyMass = mass
        , bodyRestitution = 0.18
        , bodyCCD = True
        }

fxBody :: BlastFx -> Body SiegeBody
fxBody fx =
  let ringMat =
        defaultMaterial
          { matColor = Just (fxColor fx)
          , matEmissive = Just (fxColor fx, 2.4)
          , matOpacity = 0.65
          , matCastShadow = False
          , matReceiveShadow = False
          }
      mesh =
        withMaterial ringMat $
          group
            [ sphere (fxRadius fx * 0.65)
            , rotate (Vec3 (pi / 2) 0 0) $ torus (fxRadius fx) 0.18
            ]
   in staticBody (BlastRing (fxNum fx)) (at (fxPos fx)) NoCollider mesh

activeTimers :: SiegeState -> [Timer SiegeTimer]
activeTimers ss =
  [Once (ShotExpire (shotNum s)) 4.5 | s <- Map.elems (ssShots ss)]
    <> [Once (FxExpire (fxNum f)) 0.38 | f <- Map.elems (ssFx ss)]
    <> [Once (ChainTnt tId) 0.16 | tId <- Set.toList (ssPendingChainTnt ss)]
    <> [ Once CheckOutOfAmmo 2.6
       | ssStatus ss == Playing
       , ssShotsLeft ss == 0
       , Map.null (ssShots ss)
       , Set.null (ssPendingChainTnt ss)
       ]

buildHud :: SiegeState -> FortressDef -> [HudElement]
buildHud ss fort =
  let ammoLabel = case ssAmmoKind ss of
        AmmoIron -> "Iron Cannonball"
        AmmoBomb -> "Explosive Bomb"
      leftHud =
        HudText TopLeft $
          "Fortress "
            <> T.pack (show (fortNumber fort))
            <> "/"
            <> T.pack (show (length fortresses))
            <> ": "
            <> fortName fort
            <> "   |   Idols: "
            <> T.pack (show (Set.size (ssActiveIdols ss)))
            <> "   |   Score: "
            <> T.pack (show (ssScore ss))
      camLabel = case ssCamMode ss of
        SmoothOrbit -> "Smooth Pivot"
        StepOrbit -> "Step Fixed"
      rightHud =
        HudText TopRight $
          "Ammo: "
            <> ammoLabel
            <> "   |   Shots: "
            <> T.pack (show (ssShotsLeft ss))
            <> "   |   Bombs: "
            <> T.pack (show (ssBombsLeft ss))
            <> "   |   Cam: "
            <> camLabel
      hintHud = HudText TopCenter (fortHint fort)
      controlsHud =
        HudText BottomCenter $
          "Left Click: Fire   ·   Q / Right Click: Iron/Bomb   ·   A / D: Orbit   ·   C: Smooth/Step Cam   ·   R: Retry   ·   1/2/3: Fortress"
      banner = case ssStatus ss of
        Playing -> []
        FortressCleared ->
          [ HudBanner
              "FORTRESS TOPPLED!"
              ("All Golden Idols destroyed!\nScore: " <> T.pack (show (ssScore ss)) <> "\nPress [Space] or [Enter] for the next fortress")
          ]
        OutOfAmmo ->
          [ HudBanner
              "OUT OF AMMO"
              (T.pack (show (Set.size (ssActiveIdols ss))) <> " Golden Idol(s) still standing.\nPress [R] to retry this fortress")
          ]
        CampaignWon ->
          [ HudBanner
              "SIEGE VICTORIOUS!"
              ("All 3 fortresses demolished!\nFinal Score: " <> T.pack (show (ssScore ss)) <> "\nPress [R] to play again")
          ]
   in [leftHud, rightHud, hintHud, controlsHud] <> banner
