{-# LANGUAGE OverloadedStrings #-}

-- | Step 5: Patrolling AI Targets, Waves & Incremental SceneTree
--
-- Adds:
-- * Flying AI targets steered every 2.5s by a declarative 'Every AiManeuver'
--   timer (using vector subtraction '-' and dot product '<.>').
-- * Wave progression ('Once NextWave 2.2'), score tracking, and short-lived
--   explosion spheres ('Once (FxExpire n) 0.35').
-- * 'mapTree' and 'intMapTree' (powered by 'memo') so untouched target,
--   bullet, and forest subtrees skip rendering and diffing in O(1).
module Step05_TargetsAndMemo (run) where

import Data.Bits ((.|.))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word16)
import Game.EventHorizon

data BodyTag
  = Player
  | Ground
  | Tree !Int
  | Target !Int !Int
  | Bullet !Int
  | Explosion !Int
  deriving (Eq, Ord, Show)

data TimerTag
  = AiManeuver
  | NextWave
  | BulletExpire !Int
  | FxExpire !Int
  deriving (Eq, Ord, Show)

data GameStatus
  = Flying
  | Crashed !Text
  deriving (Eq, Show)

data BulletInfo = BulletInfo
  { bulletNum :: !Int
  , bulletSpawnPose :: !Pose
  , bulletVel :: !Vec3
  }
  deriving (Eq, Show)

data TargetInfo = TargetInfo
  { targetWave :: !Int
  , targetIdx :: !Int
  , targetSpawnPose :: !Pose
  , targetSpeed :: !Double
  , targetYaw :: !Double
  , targetPitch :: !Double
  }
  deriving (Eq, Show)

data FxInfo = FxInfo
  { fxNum :: !Int
  , fxPos :: !Vec3
  , fxRadius :: !Double
  , fxColor :: !Color
  }
  deriving (Eq, Show)

data TreeSpec = TreeSpec
  { treeNum :: !Int
  , treeX :: !Double
  , treeZ :: !Double
  , treeHeight :: !Double
  , treeRadius :: !Double
  }
  deriving (Eq, Show)

data GameState = GameState
  { gsEpoch :: !Int
  , gsStatus :: !GameStatus
  , gsWave :: !Int
  , gsScore :: !Int
  , gsHeldKeys :: !(Set Key)
  , gsTargets :: !(Map (Int, Int) TargetInfo)
  , gsBullets :: !(IntMap BulletInfo)
  , gsFx :: !(IntMap FxInfo)
  , gsNextId :: !Int
  , gsWavePending :: !Bool
  }
  deriving (Eq, Show)

grpPlayer, grpBullet, grpTarget, grpWorld :: Word16
grpPlayer = 0x0001
grpBullet = 0x0002
grpTarget = 0x0004
grpWorld = 0x0008

forestTrees :: [TreeSpec]
forestTrees =
  [ TreeSpec i x z (11.0 + fromIntegral ((i * 7) `mod` 13)) (2.6 + 0.25 * fromIntegral ((i * 5) `mod` 5))
  | i <- [1 .. 42 :: Int]
  , let angle = fromIntegral i * 2.39996323
        ring = 22.0 + sqrt (fromIntegral i) * 16.5
        x = cos angle * ring
        z = sin angle * ring - 20.0
  , not (abs x < 14 && z > 18 && z < 75)
  ]

staticWorldTree :: SceneTree BodyTag
staticWorldTree = bodies (groundBody : map treeBody forestTrees)

spawnWaveTargets :: Int -> Map (Int, Int) TargetInfo
spawnWaveTargets wave =
  Map.fromList
    [ ((wave, i), TargetInfo wave i (atRot (Vec3 x y z) rot) spd yaw 0.0)
    | i <- [1 .. count]
    , let a = (fromIntegral i / fromIntegral count) * 2 * pi
          rad = 42.0 + fromIntegral ((i * 11) `mod` 35)
          x = cos a * rad
          z = sin a * rad - 25.0
          y = 20.0 + fromIntegral ((i * 5) `mod` 18)
          rot = quatFromEuler (Vec3 0 (a + pi * 0.5) 0)
          spd = 14.0 + fromIntegral wave * 1.5 + fromIntegral (i `mod` 4)
          yaw = if even i then 0.36 else (-0.36)
    ]
  where
    count = 6 + min 4 (wave - 1)

initialState :: GameState
initialState =
  GameState
    { gsEpoch = 0
    , gsStatus = Flying
    , gsWave = 1
    , gsScore = 0
    , gsHeldKeys = Set.empty
    , gsTargets = spawnWaveTargets 1
    , gsBullets = IM.empty
    , gsFx = IM.empty
    , gsNextId = 1
    , gsWavePending = False
    }

asBullet :: BodyTag -> Maybe Int
asBullet (Bullet n) = Just n
asBullet _ = Nothing

asTarget :: BodyTag -> Maybe (Int, Int)
asTarget (Target w i) = Just (w, i)
asTarget _ = Nothing

asObstacle :: BodyTag -> Maybe BodyTag
asObstacle b@(Tree _) = Just b
asObstacle Ground = Just Ground
asObstacle _ = Nothing

asHazard :: BodyTag -> Maybe BodyTag
asHazard b@(Tree _) = Just b
asHazard Ground = Just Ground
asHazard b@(Target _ _) = Just b
asHazard _ = Nothing

fireBullet :: Snapshot BodyTag -> GameState -> GameState
fireBullet snap gs =
  case lookupBody Player snap of
    Nothing -> gs
    Just st ->
      let nid = gsNextId gs
          fwd = forwardOf (bsRot st)
          spawnPos = bsPos st + 3.4 *^ fwd
          vel = 125.0 *^ fwd
          b = BulletInfo nid (atRot spawnPos (bsRot st)) vel
       in gs
            { gsBullets = IM.insert nid b (gsBullets gs)
            , gsNextId = nid + 1
            }

spawnExplosion :: Vec3 -> Double -> Color -> GameState -> GameState
spawnExplosion pos radius col gs =
  let nid = gsNextId gs
      fx = FxInfo nid pos radius col
   in gs
        { gsFx = IM.insert nid fx (gsFx gs)
        , gsNextId = nid + 1
        }

steerTarget :: Snapshot BodyTag -> TargetInfo -> TargetInfo
steerTarget snap tgt =
  case lookupBody (Target (targetWave tgt) (targetIdx tgt)) snap of
    Nothing -> tgt
    Just st ->
      let pos@(Vec3 x y z) = bsPos st
          toCenter = Vec3 0 24 (-20) - pos
          distXZ = sqrt (x * x + (z + 20) * (z + 20))
          fwd = forwardOf (bsRot st)
          right = rightOf (bsRot st)
          desiredYaw
            | distXZ > 85 = if right <.> toCenter > 0 then (-0.75) else 0.75
            | otherwise = targetYaw tgt
          desiredPitch
            | y < 17 && fwd <.> Vec3 0 1 0 < 0.25 = 0.55
            | y > 40 && fwd <.> Vec3 0 1 0 > (-0.25) = (-0.55)
            | otherwise = 0.0
       in tgt {targetYaw = desiredYaw, targetPitch = desiredPitch}

handleEvent :: Event BodyTag TimerTag -> Snapshot BodyTag -> GameState -> GameState
handleEvent ev snap gs = case ev of
  KeyDown (KeyChar 'r') ->
    initialState {gsEpoch = gsEpoch gs + 1}
  KeyDown KeySpace | gsStatus gs == Flying ->
    fireBullet snap gs
  MouseDown MouseLeft _ | gsStatus gs == Flying ->
    fireBullet snap gs
  KeyDown k ->
    gs {gsHeldKeys = Set.insert k (gsHeldKeys gs)}
  KeyUp k ->
    gs {gsHeldKeys = Set.delete k (gsHeldKeys gs)}
  TimerFired tid -> case tid of
    AiManeuver ->
      gs {gsTargets = Map.map (steerTarget snap) (gsTargets gs)}
    NextWave ->
      let nextW = gsWave gs + 1
       in gs {gsWave = nextW, gsTargets = spawnWaveTargets nextW, gsWavePending = False}
    BulletExpire n ->
      gs {gsBullets = IM.delete n (gsBullets gs)}
    FxExpire n ->
      gs {gsFx = IM.delete n (gsFx gs)}
  CollisionStart a b -> handleCollision a b snap gs
  _ -> gs

handleCollision :: BodyTag -> BodyTag -> Snapshot BodyTag -> GameState -> GameState
handleCollision a b snap gs
  | Just (bNum, tKey@(w, i)) <- matchPair asBullet asTarget a b
  , IM.member bNum (gsBullets gs) && Map.member tKey (gsTargets gs) =
      let hitPos = maybe vZero bsPos (lookupBody (Target w i) snap)
          remaining = Map.delete tKey (gsTargets gs)
          gs1 =
            gs
              { gsBullets = IM.delete bNum (gsBullets gs)
              , gsTargets = remaining
              , gsScore = gsScore gs + 100
              , gsWavePending = Map.null remaining
              }
       in spawnExplosion hitPos 3.2 orange gs1
  | Just (bNum, _) <- matchPair asBullet asObstacle a b
  , IM.member bNum (gsBullets gs) =
      let hitPos = maybe vZero bsPos (lookupBody (Bullet bNum) snap)
       in spawnExplosion hitPos 1.0 yellow (gs {gsBullets = IM.delete bNum (gsBullets gs)})
  | Just ((), obs) <- matchPair (only Player) asHazard a b
  , gsStatus gs == Flying =
      let reason = case obs of
            Tree _ -> "You crashed into a tree!"
            Ground -> "You crashed into the terrain!"
            Target _ _ -> "Mid-air collision with an enemy target!"
            _ -> "Crashed!"
          hitPos = maybe vZero bsPos (lookupBody Player snap)
       in spawnExplosion hitPos 4.2 red (gs {gsStatus = Crashed reason})
  | otherwise = gs

-- Top-level node builders so 'mapTree' and 'intMapTree' memoize cleanly!
targetNode :: (Int, Int) -> TargetInfo -> SceneTree BodyTag
targetNode _ = bodyNode . targetBody

bulletNode :: Int -> BulletInfo -> SceneTree BodyTag
bulletNode _ = bodyNode . bulletBody

fxNode :: Int -> FxInfo -> SceneTree BodyTag
fxNode _ = bodyNode . fxBody

render :: GameState -> Scene BodyTag TimerTag
render gs =
  defaultScene
    { sceneEpoch = gsEpoch gs
    , sceneGravity = Vec3 0 (-18) 0
    , sceneBackground = skyBlue
    , sceneFog = Just (Color 0.56 0.80 0.96, 85, 250)
    , sceneCamera =
        FollowCamera
          { camTarget = Player
          , camOffset = Vec3 0 3.0 10.5
          , camLookOffset = Vec3 0 0.4 (-14.0)
          , camLocal = True
          , camSmooth = 7.5
          , camFov = 62
          }
    , sceneTree =
        staticWorldTree
          <> bodyNode (playerBody gs)
          <> mapTree targetNode (gsTargets gs)
          <> intMapTree bulletNode (gsBullets gs)
          <> intMapTree fxNode (gsFx gs)
    , sceneTimers =
        [Every AiManeuver 2.5 | gsStatus gs == Flying]
          <> [Once NextWave 2.2 | gsWavePending gs]
          <> [Once (BulletExpire (bulletNum b)) 2.2 | b <- IM.elems (gsBullets gs)]
          <> [Once (FxExpire (fxNum f)) 0.35 | f <- IM.elems (gsFx gs)]
    , sceneHud = buildHud gs
    }

buildHud :: GameState -> [HudElement]
buildHud gs =
  let leftHud =
        HudText TopLeft $
          "Score: " <> T.pack (show (gsScore gs))
            <> "   |   Wave: " <> T.pack (show (gsWave gs))
            <> "   |   Targets: " <> T.pack (show (Map.size (gsTargets gs)))
      controls = HudText BottomCenter "W/S: Climb/Dive   ·   A/D: Turn   ·   Shift: Boost   ·   Space / Click: Shoot   ·   R: Restart"
   in case gsStatus gs of
        Flying ->
          [leftHud, HudCrosshair, controls]
            <> [HudText TopCenter ("Wave " <> T.pack (show (gsWave gs)) <> " cleared!") | gsWavePending gs]
        Crashed reason ->
          [ leftHud
          , controls
          , HudBanner "CRASHED" (reason <> "\nFinal Score: " <> T.pack (show (gsScore gs)) <> "\nPress [R] to fly again")
          ]

groundBody :: Body BodyTag
groundBody =
  ( staticBody
      Ground
      (at (Vec3 0 (-1) 0))
      (BoxCollider (Vec3 600 2 600))
      (colored (Color 0.24 0.52 0.24) (box (Vec3 600 2 600)))
  )
    { bodyFilter = CollisionFilter grpWorld 0xFFFF
    }

treeBody :: TreeSpec -> Body BodyTag
treeBody t =
  let h = treeHeight t
      r = treeRadius t
      trunkH = h * 0.42
      canopyH = h * 0.70
      mesh =
        group
          [ colored brown $ translate (Vec3 0 (-h * 0.5 + trunkH * 0.5) 0) $ cylinder (r * 0.22) (r * 0.30) trunkH
          , colored forestGreen $ translate (Vec3 0 (h * 0.06) 0) $ cone r (canopyH * 0.72)
          , colored (Color 0.12 0.42 0.18) $ translate (Vec3 0 (h * 0.28) 0) $ cone (r * 0.72) (canopyH * 0.55)
          ]
   in ( staticBody
          (Tree (treeNum t))
          (at (Vec3 (treeX t) (h * 0.5) (treeZ t)))
          (CylinderCollider (r * 0.58) h)
          mesh
      )
        { bodyFilter = CollisionFilter grpWorld 0xFFFF
        }

playerBody :: GameState -> Body BodyTag
playerBody gs =
  let base =
        ( dynamicBody
            Player
            (at (Vec3 0 18 52))
            (BoxCollider (Vec3 3.0 0.9 2.6))
            airplaneMesh
        )
          { bodyFilter = CollisionFilter grpPlayer (grpTarget .|. grpWorld)
          }
   in case gsStatus gs of
        Flying -> base {bodyGravity = 0.0, bodyMass = 2.0, bodyDrive = computeDrive (gsHeldKeys gs)}
        Crashed _ -> base {bodyGravity = 1.0, bodyDamping = (1.2, 1.2), bodyDrive = NoDrive}

targetBody :: TargetInfo -> Body BodyTag
targetBody tgt =
  let mesh =
        group
          [ colored red $ sphere 1.15
          , colored darkGray $ box (Vec3 4.0 0.16 0.85)
          , colored orange $ translate (Vec3 0 0.45 0.85) $ box (Vec3 0.14 0.75 0.55)
          , glow yellow 1.4 $ colored yellow $ translate (Vec3 0 0 (-1.15)) $ sphere 0.38
          ]
   in ( dynamicBody
          (Target (targetWave tgt) (targetIdx tgt))
          (targetSpawnPose tgt)
          (SphereCollider 2.1)
          mesh
      )
        { bodyGravity = 0.0
        , bodyMass = 2.0
        , bodyFilter = CollisionFilter grpTarget (grpPlayer .|. grpBullet .|. grpWorld)
        , bodyDrive = FlightDrive (targetSpeed tgt) (targetPitch tgt) (targetYaw tgt) 0.0 True
        }

bulletBody :: BulletInfo -> Body BodyTag
bulletBody b =
  ( dynamicBody
      (Bullet (bulletNum b))
      (bulletSpawnPose b)
      (SphereCollider 0.55)
      (glow yellow 2.2 $ colored yellow $ sphere 0.35)
  )
    { bodyInitialVel = bulletVel b
    , bodyGravity = 0.0
    , bodyMass = 0.1
    , bodyCCD = True
    , bodyFilter = CollisionFilter grpBullet (grpTarget .|. grpWorld)
    }

fxBody :: FxInfo -> Body BodyTag
fxBody fx =
  staticBody
    (Explosion (fxNum fx))
    (at (fxPos fx))
    NoCollider
    (glow (fxColor fx) 2.5 $ colored (fxColor fx) $ sphere (fxRadius fx))

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
   in FlightDrive speed pitch yaw 0.0 True

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
    (defaultConfig "Sky Ace Tutorial — Step 5: Targets & Memoized SceneTree") {cfgPort = port}
    initialState
    render
    handleEvent
