{-# LANGUAGE OverloadedStrings #-}

-- | Step 4: Shooting, Snapshots, Timers & Collision Filters
--
-- Adds:
-- * 'Snapshot' inspection ('lookupBody Player snap') and vector math operators
--   ('+', '*^', 'forwardOf') to spawn fast bullets ahead of the plane's nose.
-- * Type-safe 'TimerTag' with declarative 'Once' timers to despawn bullets.
-- * 'CollisionFilter' bitmasks so bullets never collide with the player.
-- * 'intMapTree' for incremental O(delta log N) rendering of active bullets.
module Step04_ShootingAndTimers (run) where

import Data.Bits ((.|.))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Word (Word16)
import Game.EventHorizon

data BodyTag
  = Player
  | Ground
  | Tree !Int
  | Bullet !Int
  deriving (Eq, Ord, Show)

data TimerTag
  = BulletExpire !Int
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
  , gsHeldKeys :: !(Set Key)
  , gsBullets :: !(IntMap BulletInfo)
  , gsNextId :: !Int
  }
  deriving (Eq, Show)

-- 16-bit collision groups: Player and Bullet do not collide with each other.
grpPlayer, grpBullet, grpWorld :: Word16
grpPlayer = 0x0001
grpBullet = 0x0002
grpWorld = 0x0004

initialState :: GameState
initialState = GameState 0 Flying Set.empty IM.empty 1

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

asBullet :: BodyTag -> Maybe Int
asBullet (Bullet n) = Just n
asBullet _ = Nothing

asObstacle :: BodyTag -> Maybe BodyTag
asObstacle b@(Tree _) = Just b
asObstacle Ground = Just Ground
asObstacle _ = Nothing

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
  TimerFired (BulletExpire n) ->
    gs {gsBullets = IM.delete n (gsBullets gs)}
  CollisionStart a b
    | Just (bNum, _) <- matchPair asBullet asObstacle a b ->
        gs {gsBullets = IM.delete bNum (gsBullets gs)}
    | Just ((), obs) <- matchPair (only Player) asObstacle a b
    , gsStatus gs == Flying ->
        let reason = case obs of
              Tree _ -> "You crashed into a tree!"
              Ground -> "You crashed into the terrain!"
              _ -> "Crashed!"
         in gs {gsStatus = Crashed reason}
  _ -> gs

bulletNode :: Int -> BulletInfo -> SceneTree BodyTag
bulletNode _ = bodyNode . bulletBody

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
          <> intMapTree bulletNode (gsBullets gs)
    , sceneTimers =
        [Once (BulletExpire (bulletNum b)) 2.2 | b <- IM.elems (gsBullets gs)]
    , sceneHud = buildHud gs
    }

buildHud :: GameState -> [HudElement]
buildHud gs =
  let header = HudText TopLeft "Step 4: Shooting, Snapshots & Timers"
      controls = HudText BottomCenter "W/S: Climb/Dive   ·   A/D: Turn   ·   Shift: Boost   ·   Space / Click: Shoot   ·   R: Restart"
   in case gsStatus gs of
        Flying -> [header, HudCrosshair, controls]
        Crashed reason ->
          [ header
          , controls
          , HudBanner "CRASHED" (reason <> "\nPress [R] to fly again")
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
          { bodyFilter = CollisionFilter grpPlayer grpWorld
          }
   in case gsStatus gs of
        Flying ->
          base
            { bodyGravity = 0.0
            , bodyMass = 2.0
            , bodyDrive = computeDrive (gsHeldKeys gs)
            }
        Crashed _ ->
          base
            { bodyGravity = 1.0
            , bodyDamping = (1.2, 1.2)
            , bodyDrive = NoDrive
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
    , bodyFilter = CollisionFilter grpBullet (grpWorld .|. grpBullet)
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
    (defaultConfig "Sky Ace Tutorial — Step 4: Shooting & Timers") {cfgPort = port}
    initialState
    render
    handleEvent
