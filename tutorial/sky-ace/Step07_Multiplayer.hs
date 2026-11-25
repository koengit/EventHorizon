{-# LANGUAGE OverloadedStrings #-}

-- | Step 7: Multiplayer (2-Player Dogfight With Zero Networking Code)
--
-- Upgrades Step 6 from 'playWith' to 'Game Pilot BodyTag TimerTag GameState':
-- * Introduces @data Pilot = Pilot1 | Pilot2@ (player ID @pid@) and moves
--   per-pilot state into @gsPilots :: Map Pilot PilotState@.
-- * Splits 'render' into shared 'renderWorld' ('WorldScene') and per-pilot
--   'renderView' ('PlayerView': camera + HUD).
-- * Adds PvP takedowns (+250 score) and 3-second 'Teleport' respawn timers.
-- * Runs identically across local browser tabs ('playLocalMulti') or over the
--   network with client-side prediction & rollback ('serveMulti' / 'connectMulti').
module Step07_Multiplayer (runLocal) where

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

data Pilot
  = Pilot1
  | Pilot2
  deriving (Eq, Ord, Show, Enum, Bounded)

data BodyTag
  = Player
  | Player2
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
  | PilotRespawn !Pilot
  deriving (Eq, Ord, Show)

data GameStatus
  = Flying
  | Crashed !Text
  deriving (Eq, Show)

data PilotState = PilotState
  { psStatus :: !GameStatus
  , psScore :: !Int
  , psHeldKeys :: !(Set Key)
  }
  deriving (Eq, Show)

data BulletInfo = BulletInfo
  { bulletNum :: !Int
  , bulletOwner :: !Pilot
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
  , gsWave :: !Int
  , gsPilots :: !(Map Pilot PilotState)
  , gsTargets :: !(Map (Int, Int) TargetInfo)
  , gsBullets :: !(IntMap BulletInfo)
  , gsFx :: !(IntMap FxInfo)
  , gsNextId :: !Int
  , gsWavePending :: !Bool
  }
  deriving (Eq, Show)

defaultPilotState :: PilotState
defaultPilotState = PilotState Flying 0 Set.empty

pilotBody :: Pilot -> BodyTag
pilotBody Pilot1 = Player
pilotBody Pilot2 = Player2

pilotName :: Pilot -> Text
pilotName Pilot1 = "Pilot 1 (Blue)"
pilotName Pilot2 = "Pilot 2 (Crimson)"

pilotSpawnPose :: Pilot -> Pose
pilotSpawnPose Pilot1 = at (Vec3 0 18 52)
pilotSpawnPose Pilot2 = at (Vec3 16 18 52)

pilotState :: Pilot -> GameState -> PilotState
pilotState p gs = Map.findWithDefault defaultPilotState p (gsPilots gs)

updatePilot :: Pilot -> (PilotState -> PilotState) -> GameState -> GameState
updatePilot p f gs = gs {gsPilots = Map.adjust f p (gsPilots gs)}

grpPilot1, grpPilot2, grpBullet1, grpBullet2, grpTarget, grpWorld :: Word16
grpPilot1 = 0x0001
grpBullet1 = 0x0002
grpTarget = 0x0004
grpWorld = 0x0008
grpPilot2 = 0x0010
grpBullet2 = 0x0020

forestTrees :: [TreeSpec]
forestTrees =
  [ TreeSpec i x z (11.0 + fromIntegral ((i * 7) `mod` 13)) (2.6 + 0.25 * fromIntegral ((i * 5) `mod` 5))
  | i <- [1 .. 42 :: Int]
  , let angle = fromIntegral i * 2.39996323
        ring = 22.0 + sqrt (fromIntegral i) * 16.5
        x = cos angle * ring
        z = sin angle * ring - 20.0
  , not (x > (-14) && x < 28 && z > 18 && z < 75)
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
    , gsWave = 1
    , gsPilots = Map.fromList [(Pilot1, defaultPilotState), (Pilot2, defaultPilotState)]
    , gsTargets = spawnWaveTargets 1
    , gsBullets = IM.empty
    , gsFx = IM.empty
    , gsNextId = 1
    , gsWavePending = False
    }

-- | Pure multi-user Game value: completely oblivious to sockets or rollback!
multiGame :: Game Pilot BodyTag TimerTag GameState
multiGame =
  Game
    { gameInit = initialState
    , gameWorld = renderWorld
    , gameView = renderView
    , gameStep = stepMulti
    }

asPilot :: BodyTag -> Maybe Pilot
asPilot Player = Just Pilot1
asPilot Player2 = Just Pilot2
asPilot _ = Nothing

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

fireBullet :: Pilot -> Snapshot BodyTag -> GameState -> (GameState, [MultiCommand Pilot BodyTag])
fireBullet owner snap gs =
  case lookupBody (pilotBody owner) snap of
    Nothing -> (gs, [])
    Just st ->
      let nid = gsNextId gs
          fwd = forwardOf (bsRot st)
          spawnPos = bsPos st + 3.4 *^ fwd
          vel = 125.0 *^ fwd
          b = BulletInfo nid owner (atRot spawnPos (bsRot st)) vel
          laserSfx = WorldCmd (PlaySound (sweep WaveSawtooth 680 180 0.14 0.16))
       in (gs {gsBullets = IM.insert nid b (gsBullets gs), gsNextId = nid + 1}, [laserSfx])

spawnExplosion :: Vec3 -> Double -> Color -> GameState -> GameState
spawnExplosion pos radius col gs =
  let nid = gsNextId gs
      fx = FxInfo nid pos radius col
   in gs {gsFx = IM.insert nid fx (gsFx gs), gsNextId = nid + 1}

crashPilot :: Pilot -> Text -> Snapshot BodyTag -> GameState -> (GameState, [MultiCommand Pilot BodyTag])
crashPilot p reason snap gs =
  case psStatus (pilotState p gs) of
    Crashed _ -> (gs, [])
    Flying ->
      let hitPos = maybe vZero bsPos (lookupBody (pilotBody p) snap)
          gs1 = updatePilot p (\ps -> ps {psStatus = Crashed reason}) gs
          crashSfx =
            [ WorldCmd (PlaySound (noise 0.55 0.40))
            , WorldCmd (PlaySound (sweep WaveSawtooth 160 40 0.55 0.28))
            ]
       in (spawnExplosion hitPos 4.2 red gs1, crashSfx)

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

stepMulti :: MultiEvent Pilot BodyTag TimerTag -> Snapshot BodyTag -> GameState -> (GameState, [MultiCommand Pilot BodyTag])
stepMulti ev snap gs = case ev of
  PlayerJoined p ->
    (gs {gsPilots = Map.insertWith (\_ old -> old) p defaultPilotState (gsPilots gs)}, [])
  PlayerLeft p ->
    (gs {gsPilots = Map.delete p (gsPilots gs)}, [])
  PlayerInput p inp -> case inp of
    InKeyDown (KeyChar 'r') ->
      (initialState {gsEpoch = gsEpoch gs + 1, gsPilots = Map.map (const defaultPilotState) (gsPilots gs)}, [])
    InKeyDown KeySpace | psStatus (pilotState p gs) == Flying ->
      fireBullet p snap gs
    InMouseDown MouseLeft _ | psStatus (pilotState p gs) == Flying ->
      fireBullet p snap gs
    InKeyDown k ->
      (updatePilot p (\ps -> ps {psHeldKeys = Set.insert k (psHeldKeys ps)}) gs, [])
    InKeyUp k ->
      (updatePilot p (\ps -> ps {psHeldKeys = Set.delete k (psHeldKeys ps)}) gs, [])
    _ -> (gs, [])
  WorldTimerFired tid -> case tid of
    AiManeuver -> (gs {gsTargets = Map.map (steerTarget snap) (gsTargets gs)}, [])
    NextWave ->
      let nextW = gsWave gs + 1
       in (gs {gsWave = nextW, gsTargets = spawnWaveTargets nextW, gsWavePending = False}, [])
    BulletExpire n -> (gs {gsBullets = IM.delete n (gsBullets gs)}, [])
    FxExpire n -> (gs {gsFx = IM.delete n (gsFx gs)}, [])
    PilotRespawn p ->
      ( updatePilot p (\ps -> ps {psStatus = Flying, psHeldKeys = Set.empty}) gs
      , [ WorldCmd (Teleport (pilotBody p) (pilotSpawnPose p))
        , WorldCmd (SetVelocity (pilotBody p) vZero)
        , WorldCmd (SetAngularVelocity (pilotBody p) vZero)
        , PlayerCmd p (PlaySound (sweep WaveSine 330 660 0.25 0.18))
        ]
      )
  WorldCollisionStart a b -> handleCollision a b snap gs
  WorldCollisionEnd _ _ -> (gs, [])

handleCollision :: BodyTag -> BodyTag -> Snapshot BodyTag -> GameState -> (GameState, [MultiCommand Pilot BodyTag])
handleCollision a b snap gs
  | Just (bNum, tKey@(w, i)) <- matchPair asBullet asTarget a b
  , Just bInfo <- IM.lookup bNum (gsBullets gs)
  , Map.member tKey (gsTargets gs) =
      let hitPos = maybe vZero bsPos (lookupBody (Target w i) snap)
          remaining = Map.delete tKey (gsTargets gs)
          cleared = Map.null remaining
          gs1 =
            updatePilot
              (bulletOwner bInfo)
              (\ps -> ps {psScore = psScore ps + 100})
              gs {gsBullets = IM.delete bNum (gsBullets gs), gsTargets = remaining, gsWavePending = cleared}
          sfx =
            [ WorldCmd (PlaySound (noise 0.28 0.30))
            , WorldCmd (PlaySound (sweep WaveTriangle 240 60 0.32 0.24))
            ]
              <> [WorldCmd (PlaySound (sweep WaveSine 440 880 0.35 0.22)) | cleared]
       in (spawnExplosion hitPos 3.2 orange gs1, sfx)
  | Just (bNum, victim) <- matchPair asBullet asPilot a b
  , Just bInfo <- IM.lookup bNum (gsBullets gs)
  , bulletOwner bInfo /= victim && psStatus (pilotState victim gs) == Flying =
      let shooter = bulletOwner bInfo
          gs0 =
            updatePilot
              shooter
              (\ps -> ps {psScore = psScore ps + 250})
              (gs {gsBullets = IM.delete bNum (gsBullets gs)})
       in crashPilot victim ("Shot down by " <> pilotName shooter <> "!") snap gs0
  | Just (bNum, _) <- matchPair asBullet asObstacle a b
  , IM.member bNum (gsBullets gs) =
      let hitPos = maybe vZero bsPos (lookupBody (Bullet bNum) snap)
       in ( spawnExplosion hitPos 1.0 yellow (gs {gsBullets = IM.delete bNum (gsBullets gs)})
          , [WorldCmd (PlaySound (noise 0.08 0.12))]
          )
  | Just p1 <- asPilot a
  , Just p2 <- asPilot b
  , p1 /= p2 =
      let (gs1, s1) = crashPilot p1 "Mid-air collision with rival pilot!" snap gs
          (gs2, s2) = crashPilot p2 "Mid-air collision with rival pilot!" snap gs1
       in (gs2, s1 <> s2)
  | Just (p, obs) <- matchPair asPilot asHazard a b =
      let reason = case obs of
            Tree _ -> "You crashed into a tree!"
            Ground -> "You crashed into the terrain!"
            Target _ _ -> "Mid-air collision with an enemy target!"
            _ -> "Crashed!"
       in crashPilot p reason snap gs
  | otherwise = (gs, [])

pilotNode :: Pilot -> PilotState -> SceneTree BodyTag
pilotNode p ps = bodyNode (playerBody p ps)

targetNode :: (Int, Int) -> TargetInfo -> SceneTree BodyTag
targetNode _ = bodyNode . targetBody

bulletNode :: Int -> BulletInfo -> SceneTree BodyTag
bulletNode _ = bodyNode . bulletBody

fxNode :: Int -> FxInfo -> SceneTree BodyTag
fxNode _ = bodyNode . fxBody

-- | Shared 3D world rendered once for all connected players.
renderWorld :: GameState -> WorldScene BodyTag TimerTag
renderWorld gs =
  let anyFlying = any ((== Flying) . psStatus) (Map.elems (gsPilots gs))
      respawns = [Once (PilotRespawn p) 3.0 | (p, ps) <- Map.toAscList (gsPilots gs), psStatus ps /= Flying]
   in defaultWorldScene
        { wsEpoch = gsEpoch gs
        , wsGravity = Vec3 0 (-18) 0
        , wsBackground = Color 0.46 0.75 0.96
        , wsFog = Just (Color 0.56 0.80 0.96, 85, 250)
        , wsLights =
            [ HemisphereLight (Color 0.72 0.88 1.0) (Color 0.24 0.36 0.18) 0.75
            , DirectionalLight (Color 1.0 0.97 0.88) 1.3 (Vec3 35 60 25)
            ]
        , wsTree =
            staticWorldTree
              <> mapTree pilotNode (gsPilots gs)
              <> mapTree targetNode (gsTargets gs)
              <> intMapTree bulletNode (gsBullets gs)
              <> intMapTree fxNode (gsFx gs)
        , wsTimers =
            [Every AiManeuver 2.5 | anyFlying]
              <> [Once NextWave 2.2 | gsWavePending gs]
              <> respawns
              <> [Once (BulletExpire (bulletNum b)) 2.2 | b <- IM.elems (gsBullets gs)]
              <> [Once (FxExpire (fxNum f)) 0.35 | f <- IM.elems (gsFx gs)]
        }

-- | Per-player camera & HUD overlay.
renderView :: Pilot -> GameState -> PlayerView BodyTag
renderView p gs =
  PlayerView
    { pvCamera =
        FollowCamera
          { camTarget = pilotBody p
          , camOffset = Vec3 0 3.0 10.5
          , camLookOffset = Vec3 0 0.4 (-14.0)
          , camLocal = True
          , camSmooth = 7.5
          , camFov = 62
          }
    , pvHud = buildHud p gs
    }

buildHud :: Pilot -> GameState -> [HudElement]
buildHud p gs =
  let myState = pilotState p gs
      rival = if p == Pilot1 then Pilot2 else Pilot1
      leftHud =
        HudText TopLeft $
          pilotName p <> ": " <> T.pack (show (psScore myState))
            <> "   |   " <> pilotName rival <> ": " <> T.pack (show (psScore (pilotState rival gs)))
            <> "   |   Wave: " <> T.pack (show (gsWave gs))
      controls = HudText BottomCenter "W/S: Climb/Dive   ·   A/D: Turn   ·   Shift: Boost   ·   Space / Click: Shoot   ·   R: Restart"
   in case psStatus myState of
        Flying -> [leftHud, HudCrosshair, controls]
        Crashed reason ->
          [ leftHud
          , controls
          , HudBanner "DOWNED" (reason <> "\nScore: " <> T.pack (show (psScore myState)) <> "\nRespawning in 3 seconds...")
          ]

noShadowGlow :: Color -> Double -> Material
noShadowGlow col intensity =
  defaultMaterial {matColor = Just col, matEmissive = Just (col, intensity), matCastShadow = False, matReceiveShadow = False}

groundBody :: Body BodyTag
groundBody =
  (staticBody Ground (at (Vec3 0 (-1) 0)) (BoxCollider (Vec3 600 2 600)) (colored (Color 0.24 0.52 0.24) (box (Vec3 600 2 600))))
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
   in (staticBody (Tree (treeNum t)) (at (Vec3 (treeX t) (h * 0.5) (treeZ t))) (CylinderCollider (r * 0.58) h) mesh)
        { bodyFilter = CollisionFilter grpWorld 0xFFFF
        }

playerBody :: Pilot -> PilotState -> Body BodyTag
playerBody p ps =
  let (grp, mask) = case p of
        Pilot1 -> (grpPilot1, grpPilot2 .|. grpBullet2 .|. grpTarget .|. grpWorld)
        Pilot2 -> (grpPilot2, grpPilot1 .|. grpBullet1 .|. grpTarget .|. grpWorld)
      base =
        (dynamicBody (pilotBody p) (pilotSpawnPose p) (BoxCollider (Vec3 3.0 0.9 2.6)) (airplaneMeshFor p))
          { bodyFilter = CollisionFilter grp mask
          }
   in case psStatus ps of
        Flying -> base {bodyGravity = 0.0, bodyMass = 2.0, bodyDrive = computeDrive (psHeldKeys ps)}
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
   in (dynamicBody (Target (targetWave tgt) (targetIdx tgt)) (targetSpawnPose tgt) (SphereCollider 2.1) mesh)
        { bodyGravity = 0.0
        , bodyMass = 2.0
        , bodyFilter = CollisionFilter grpTarget (grpPilot1 .|. grpPilot2 .|. grpBullet1 .|. grpBullet2 .|. grpWorld)
        , bodyDrive = FlightDrive (targetSpeed tgt) (targetPitch tgt) (targetYaw tgt) 0.0 True
        }

bulletBody :: BulletInfo -> Body BodyTag
bulletBody b =
  let (col, grp, mask) = case bulletOwner b of
        Pilot1 -> (yellow, grpBullet1, grpPilot2 .|. grpTarget .|. grpWorld)
        Pilot2 -> (cyan, grpBullet2, grpPilot1 .|. grpTarget .|. grpWorld)
   in (dynamicBody (Bullet (bulletNum b)) (bulletSpawnPose b) (SphereCollider 0.55) (withMaterial (noShadowGlow col 2.2) (sphere 0.35)))
        { bodyInitialVel = bulletVel b
        , bodyGravity = 0.0
        , bodyMass = 0.1
        , bodyCCD = True
        , bodyFilter = CollisionFilter grp mask
        }

fxBody :: FxInfo -> Body BodyTag
fxBody fx =
  staticBody (Explosion (fxNum fx)) (at (fxPos fx)) NoCollider (withMaterial (noShadowGlow (fxColor fx) 2.5) (sphere (fxRadius fx)))

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

airplaneMeshFor :: Pilot -> Mesh
airplaneMeshFor p =
  let (fuselageCol, accentCol) = case p of
        Pilot1 -> (Color 0.20 0.52 0.88, red)
        Pilot2 -> (Color 0.82 0.14 0.18, gold)
   in group
        [ colored fuselageCol $ box (Vec3 0.72 0.55 2.9)
        , colored gold $ translate (Vec3 0 0 (-1.75)) $ rotate (Vec3 (-pi / 2) 0 0) $ cone 0.36 0.75
        , glow cyan 0.35 $ colored cyan $ translate (Vec3 0 0.35 (-0.25)) $ sphere 0.34
        , colored white $ translate (Vec3 0 (-0.02) 0.1) $ box (Vec3 4.4 0.12 0.95)
        , colored accentCol $ translate (Vec3 (-2.1) 0 0.1) $ box (Vec3 0.35 0.16 0.98)
        , colored accentCol $ translate (Vec3 2.1 0 0.1) $ box (Vec3 0.35 0.16 0.98)
        , colored white $ translate (Vec3 0 0.08 1.25) $ box (Vec3 1.7 0.10 0.45)
        , colored accentCol $ translate (Vec3 0 0.52 1.2) $ box (Vec3 0.12 0.82 0.58)
        , glow orange 1.5 $ colored orange $ translate (Vec3 0 0 1.52) $ sphere 0.24
        ]

runLocal :: Int -> IO ()
runLocal port =
  playLocalMulti
    (defaultConfig "Sky Ace Tutorial — Step 7: 2-Player Dogfight") {cfgPort = port}
    multiGame
