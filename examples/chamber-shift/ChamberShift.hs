{-# LANGUAGE OverloadedStrings #-}

module ChamberShift
  ( PlateId (..)
  , ButtonId (..)
  , DoorId (..)
  , SwitchId (..)
  , PuzzleBody (..)
  , PuzzleTimer (..)
  , PuzzleState (..)
  , LevelDef (..)
  , WallDef (..)
  , CrateDef (..)
  , PlateDef (..)
  , ButtonDef (..)
  , ButtonMode (..)
  , DoorDef (..)
  , DoorRule (..)
  , levels
  , initialPuzzle
  , renderPuzzle
  , stepPuzzle
  , handleEvent
  , evalRule
  , isDoorOpen
  ) where

import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Game.EventHorizon

-- | Type-safe identifiers for pressure plates across all levels.
data PlateId
  = Plate1
  | PlateA
  | PlateB
  | PlateBridge
  | PlateExit1
  | PlateExit2
  deriving (Eq, Ord, Show)

-- | Type-safe identifiers for pedestal buttons across all levels.
data ButtonId
  = BtnGate
  | BtnVaultOut
  | BtnVaultIn
  | BtnAirlock
  | BtnMidTimer
  deriving (Eq, Ord, Show)

-- | Type-safe identifiers for sliding doors across all levels.
data DoorId
  = Door1
  | Door2
  | DoorVault
  | DoorExit
  | DoorWest
  | DoorEast
  | DoorMid
  | DoorFinal
  deriving (Eq, Ord, Show)

-- | Type-safe signal source that can control a door rule.
data SwitchId
  = PlateSwitch !PlateId
  | ButtonSwitch !ButtonId
  deriving (Eq, Ord, Show)

-- | Type-safe 3D body identifiers for Chamber Shift.
data PuzzleBody
  = Player
  | Floor
  | Exit
  | Wall !Int
  | Crate !Int
  | Plate !PlateId
  | Button !ButtonId
  | ButtonZone !ButtonId
  | Door !DoorId
  deriving (Eq, Ord, Show)

-- | Type-safe timer identifiers for Chamber Shift.
data PuzzleTimer
  = ButtonTimeout !ButtonId !Int
  deriving (Eq, Ord, Show)

data ButtonMode
  = ToggleButton
  | TimedButton !Double
  deriving (Eq, Show)

data DoorRule
  = WhenActive !SwitchId
  | WhenNot !SwitchId
  | WhenAll ![DoorRule]
  | WhenAny ![DoorRule]
  deriving (Eq, Show)

data WallDef = WallDef
  { wallNum :: !Int
  , wallPos :: !Vec3
  , wallSize :: !Vec3
  }
  deriving (Eq, Show)

data CrateDef = CrateDef
  { crateNum :: !Int
  , cratePos :: !Vec3
  }
  deriving (Eq, Show)

data PlateDef = PlateDef
  { plateId :: !PlateId
  , platePos :: !Vec3
  , plateLabel :: !Text
  }
  deriving (Eq, Show)

data ButtonDef = ButtonDef
  { btnId :: !ButtonId
  , btnPos :: !Vec3
  , btnMode :: !ButtonMode
  , btnLabel :: !Text
  }
  deriving (Eq, Show)

data DoorDef = DoorDef
  { doorId :: !DoorId
  , doorClosedPos :: !Vec3
  , doorSize :: !Vec3
  , doorRule :: !DoorRule
  }
  deriving (Eq, Show)

data LevelDef = LevelDef
  { lvlNumber :: !Int
  , lvlName :: !Text
  , lvlHint :: !Text
  , lvlFloorSize :: !(Double, Double)
  , lvlPlayerStart :: !Vec3
  , lvlExitPos :: !Vec3
  , lvlWalls :: ![WallDef]
  , lvlCrates :: ![CrateDef]
  , lvlPlates :: ![PlateDef]
  , lvlButtons :: ![ButtonDef]
  , lvlDoors :: ![DoorDef]
  }
  deriving (Eq, Show)

data PuzzleState = PuzzleState
  { psEpoch :: !Int
  , psLevelIndex :: !Int
  , psWon :: !Bool
  , psHeldKeys :: !(Set Key)
  , psPlateOccupants :: !(Map PlateId (Set PuzzleBody))
  , psActiveButtons :: !(Set ButtonId)
  , psButtonTimers :: !(Map ButtonId (Int, Double))
  , psNearbyButtons :: !(Set ButtonId)
  , psTimerNonce :: !Int
  }
  deriving (Eq, Show)

-- | Three handcrafted, progressively deeper 3D physics puzzle levels.
levels :: [LevelDef]
levels = [level1, level2, level3]

-- | Precomputed static geometry (floor + walls), crates, and exit per level.
-- Reused verbatim across frames so EventHorizon skips unchanged subtrees in O(1).
levelStaticTrees :: [SceneTree PuzzleBody]
levelStaticTrees =
  [ let (fw, fd) = lvlFloorSize lvl
     in bodies (floorBody fw fd : map wallBody (lvlWalls lvl))
  | lvl <- levels
  ]

levelCrateTrees :: [SceneTree PuzzleBody]
levelCrateTrees =
  [ bodies (map crateBody (lvlCrates lvl))
  | lvl <- levels
  ]

levelExitTrees :: [SceneTree PuzzleBody]
levelExitTrees =
  [ bodyNode (exitBody (lvlExitPos lvl))
  | lvl <- levels
  ]

wall :: Int -> Double -> Double -> Double -> Double -> WallDef
wall n x z sx sz = WallDef n (Vec3 x 1.6 z) (Vec3 sx 3.2 sz)

-- | Level 1: Weight & Switch
level1 :: LevelDef
level1 =
  LevelDef
    { lvlNumber = 1
    , lvlName = "Weight & Switch"
    , lvlHint = "Press [E] (or click) at the pedestal button to open the first door, then push the crate onto the floor plate."
    , lvlFloorSize = (20, 20)
    , lvlPlayerStart = Vec3 0 1.0 6.5
    , lvlExitPos = Vec3 0 0.12 (-7.8)
    , lvlWalls =
        [ wall 1 0 (-9.5) 20 1
        , wall 2 0 9.5 20 1
        , wall 3 (-9.5) 0 1 20
        , wall 4 9.5 0 1 20
        , wall 5 (-5.75) 1 7.5 0.9
        , wall 6 5.75 1 7.5 0.9
        , wall 7 (-5.75) (-5.5) 7.5 0.9
        , wall 8 5.75 (-5.5) 7.5 0.9
        ]
    , lvlCrates =
        [ CrateDef 1 (Vec3 4.2 0.72 4.8)
        ]
    , lvlPlates =
        [ PlateDef Plate1 (Vec3 (-4.5) 0.08 (-2.3)) "Exit Plate"
        ]
    , lvlButtons =
        [ ButtonDef BtnGate (Vec3 (-5.2) 0 5.2) ToggleButton "Gate Switch"
        ]
    , lvlDoors =
        [ DoorDef Door1 (Vec3 0 1.6 1) (Vec3 3.9 3.1 0.65) (WhenActive (ButtonSwitch BtnGate))
        , DoorDef Door2 (Vec3 0 1.6 (-5.5)) (Vec3 3.9 3.1 0.65) (WhenActive (PlateSwitch Plate1))
        ]
    }

-- | Level 2: Double Lock & Timed Vault
level2 :: LevelDef
level2 =
  LevelDef
    { lvlNumber = 2
    , lvlName = "Double Lock & Timed Vault"
    , lvlHint = "Both floor plates must be held down. Use the timed vault button [E] to retrieve the second crate!"
    , lvlFloorSize = (24, 22)
    , lvlPlayerStart = Vec3 (-4.5) 1.0 7.5
    , lvlExitPos = Vec3 (-4.0) 0.12 (-8.3)
    , lvlWalls =
        [ wall 1 0 (-10.5) 24 1
        , wall 2 0 10.5 24 1
        , wall 3 (-11.5) 0 1 22
        , wall 4 11.5 0 1 22
        , wall 5 (-8.75) (-6) 5.5 0.9
        , wall 6 4.75 (-6) 13.5 0.9
        , wall 7 3 (-3.5) 0.9 5.0
        , wall 8 3 6.75 0.9 7.5
        ]
    , lvlCrates =
        [ CrateDef 1 (Vec3 (-7.5) 0.72 3.2)
        , CrateDef 2 (Vec3 7.8 0.72 1.0)
        ]
    , lvlPlates =
        [ PlateDef PlateA (Vec3 (-7.5) 0.08 (-3.2)) "Lock A"
        , PlateDef PlateB (Vec3 (-0.5) 0.08 (-3.2)) "Lock B"
        ]
    , lvlButtons =
        [ ButtonDef BtnVaultOut (Vec3 (-0.5) 0 5.5) (TimedButton 9.0) "Vault Timer (9s)"
        , ButtonDef BtnVaultIn (Vec3 8.0 0 (-3.2)) (TimedButton 9.0) "Interior Release (9s)"
        ]
    , lvlDoors =
        [ DoorDef
            DoorVault
            (Vec3 3 1.6 1)
            (Vec3 0.65 3.1 3.9)
            (WhenAny [WhenActive (ButtonSwitch BtnVaultOut), WhenActive (ButtonSwitch BtnVaultIn)])
        , DoorDef
            DoorExit
            (Vec3 (-4.0) 1.6 (-6))
            (Vec3 3.9 3.1 0.65)
            (WhenAll [WhenActive (PlateSwitch PlateA), WhenActive (PlateSwitch PlateB)])
        ]
    }

-- | Level 3: The Airlock Relay
level3 :: LevelDef
level3 =
  LevelDef
    { lvlNumber = 3
    , lvlName = "The Airlock Relay"
    , lvlHint = "The airlock toggles West/East gates. Use the bridge plate & timed inner release to bring both crates to the final lock!"
    , lvlFloorSize = (26, 22)
    , lvlPlayerStart = Vec3 0 1.0 7.5
    , lvlExitPos = Vec3 5.5 0.12 (-8.3)
    , lvlWalls =
        [ wall 1 0 (-10.5) 26 1
        , wall 2 0 10.5 26 1
        , wall 3 (-12.5) 0 1 22
        , wall 4 12.5 0 1 22
        , wall 5 (-10.25) 2.5 4.5 0.9
        , wall 6 0 2.5 8.0 0.9
        , wall 7 10.25 2.5 4.5 0.9
        , wall 8 (-1) (-4.75) 0.9 2.5
        , wall 9 (-1) 1.5 0.9 2.0
        , wall 10 (-4.5) (-6) 16.0 0.9
        , wall 11 10.0 (-6) 5.0 0.9
        ]
    , lvlCrates =
        [ CrateDef 1 (Vec3 (-6.0) 0.72 6.0)
        , CrateDef 2 (Vec3 8.2 0.72 (-0.5))
        ]
    , lvlPlates =
        [ PlateDef PlateBridge (Vec3 (-8.2) 0.08 (-1.5)) "Inner Bridge Plate"
        , PlateDef PlateExit1 (Vec3 3.0 0.08 (-3.8)) "Final Lock 1"
        , PlateDef PlateExit2 (Vec3 8.0 0.08 (-3.8)) "Final Lock 2"
        ]
    , lvlButtons =
        [ ButtonDef BtnAirlock (Vec3 0 0 5.4) ToggleButton "Airlock Toggle"
        , ButtonDef BtnMidTimer (Vec3 2.0 0 0.4) (TimedButton 11.0) "Inner Passage Timer (11s)"
        ]
    , lvlDoors =
        [ DoorDef DoorWest (Vec3 (-6) 1.6 2.5) (Vec3 3.9 3.1 0.65) (WhenNot (ButtonSwitch BtnAirlock))
        , DoorDef DoorEast (Vec3 6 1.6 2.5) (Vec3 3.9 3.1 0.65) (WhenActive (ButtonSwitch BtnAirlock))
        , DoorDef
            DoorMid
            (Vec3 (-1) 1.6 (-1.5))
            (Vec3 0.65 3.1 3.9)
            (WhenAny [WhenActive (PlateSwitch PlateBridge), WhenActive (ButtonSwitch BtnMidTimer)])
        , DoorDef
            DoorFinal
            (Vec3 5.5 1.6 (-6))
            (Vec3 3.9 3.1 0.65)
            (WhenAll [WhenActive (PlateSwitch PlateExit1), WhenActive (PlateSwitch PlateExit2)])
        ]
    }

initialPuzzle :: PuzzleState
initialPuzzle = startLevel 0 0

startLevel :: Int -> Int -> PuzzleState
startLevel epoch idx =
  PuzzleState
    { psEpoch = epoch
    , psLevelIndex = max 0 (min (length levels - 1) idx)
    , psWon = False
    , psHeldKeys = Set.empty
    , psPlateOccupants = Map.empty
    , psActiveButtons = Set.empty
    , psButtonTimers = Map.empty
    , psNearbyButtons = Set.empty
    , psTimerNonce = 0
    }

currentLevel :: PuzzleState -> LevelDef
currentLevel ps = levels !! psLevelIndex ps

isPlatePressed :: PuzzleState -> PlateId -> Bool
isPlatePressed ps pid =
  maybe False (not . Set.null) (Map.lookup pid (psPlateOccupants ps))

isSwitchActive :: PuzzleState -> SwitchId -> Bool
isSwitchActive ps sw = case sw of
  PlateSwitch pid -> isPlatePressed ps pid
  ButtonSwitch bid -> Set.member bid (psActiveButtons ps)

evalRule :: PuzzleState -> DoorRule -> Bool
evalRule ps rule = case rule of
  WhenActive sw -> isSwitchActive ps sw
  WhenNot sw -> not (isSwitchActive ps sw)
  WhenAll rs -> all (evalRule ps) rs
  WhenAny rs -> any (evalRule ps) rs

isDoorOpen :: PuzzleState -> DoorDef -> Bool
isDoorOpen ps d = evalRule ps (doorRule d)

-- | Pure event handler returning updated state + procedural audio commands.
stepPuzzle
  :: Event PuzzleBody PuzzleTimer
  -> Snapshot PuzzleBody
  -> PuzzleState
  -> (PuzzleState, [Command PuzzleBody])
stepPuzzle ev _snap ps = case ev of
  EngineReady -> (ps, [])
  KeyDown (KeyChar 'r') ->
    let next =
          if psWon ps
            then startLevel (psEpoch ps + 1) 0
            else startLevel (psEpoch ps + 1) (psLevelIndex ps)
     in (next, [])
  KeyDown (KeyChar '1') -> (startLevel (psEpoch ps + 1) 0, [])
  KeyDown (KeyChar '2') -> (startLevel (psEpoch ps + 1) 1, [])
  KeyDown (KeyChar '3') -> (startLevel (psEpoch ps + 1) 2, [])
  KeyDown (KeyChar 'e') ->
    if psWon ps then (ps, []) else interactNearbyButtons ps
  MouseDown MouseLeft (Just (Button bid, _)) ->
    if psWon ps || not (Set.member bid (psNearbyButtons ps))
      then (ps, [])
      else
        case filter (\b -> btnId b == bid) (lvlButtons (currentLevel ps)) of
          (b : _) -> (pressButton ps b, [PlaySound (sweep WaveSine 520 740 0.10 0.16)])
          [] -> (ps, [])
  MouseDown _ _ -> (ps, [])
  MouseUp _ -> (ps, [])
  KeyDown k ->
    (ps {psHeldKeys = Set.insert k (psHeldKeys ps)}, [])
  KeyUp k ->
    (ps {psHeldKeys = Set.delete k (psHeldKeys ps)}, [])
  TimerFired (ButtonTimeout bid nonce) ->
    expireButtonTimer bid nonce ps
  CollisionStart a b ->
    if psWon ps then (ps, []) else handleCollisionStart a b ps
  CollisionEnd a b ->
    if psWon ps then (ps, []) else handleCollisionEnd a b ps

-- | Pure state-only wrapper.
handleEvent :: Event PuzzleBody PuzzleTimer -> Snapshot PuzzleBody -> PuzzleState -> PuzzleState
handleEvent ev snap ps = fst (stepPuzzle ev snap ps)

interactNearbyButtons :: PuzzleState -> (PuzzleState, [Command PuzzleBody])
interactNearbyButtons ps =
  let lvl = currentLevel ps
      btns = filter (\b -> Set.member (btnId b) (psNearbyButtons ps)) (lvlButtons lvl)
   in if null btns
        then (ps, [])
        else
          ( foldl' pressButton ps btns
          , [PlaySound (sweep WaveSine 520 740 0.10 0.16)]
          )

pressButton :: PuzzleState -> ButtonDef -> PuzzleState
pressButton ps btn =
  case btnMode btn of
    ToggleButton ->
      let bid = btnId btn
          next =
            if Set.member bid (psActiveButtons ps)
              then Set.delete bid (psActiveButtons ps)
              else Set.insert bid (psActiveButtons ps)
       in ps {psActiveButtons = next}
    TimedButton secs ->
      let bid = btnId btn
          n = psTimerNonce ps + 1
       in ps
            { psActiveButtons = Set.insert bid (psActiveButtons ps)
            , psButtonTimers = Map.insert bid (n, secs) (psButtonTimers ps)
            , psTimerNonce = n
            }

expireButtonTimer :: ButtonId -> Int -> PuzzleState -> (PuzzleState, [Command PuzzleBody])
expireButtonTimer bid nonce ps =
  case Map.lookup bid (psButtonTimers ps) of
    Just (activeNonce, _) | activeNonce == nonce ->
      ( ps
          { psActiveButtons = Set.delete bid (psActiveButtons ps)
          , psButtonTimers = Map.delete bid (psButtonTimers ps)
          }
      , [PlaySound (sweep WaveSine 420 260 0.14 0.14)]
      )
    _ -> (ps, [])

asPlate :: PuzzleBody -> Maybe PlateId
asPlate (Plate pid) = Just pid
asPlate _ = Nothing

asButtonZone :: PuzzleBody -> Maybe ButtonId
asButtonZone (ButtonZone bid) = Just bid
asButtonZone _ = Nothing

asWeightActor :: PuzzleBody -> Maybe PuzzleBody
asWeightActor Player = Just Player
asWeightActor b@(Crate _) = Just b
asWeightActor _ = Nothing

handleCollisionStart
  :: PuzzleBody
  -> PuzzleBody
  -> PuzzleState
  -> (PuzzleState, [Command PuzzleBody])
handleCollisionStart a b ps
  | matchEq Player Exit a b =
      let fanfare = PlaySound (sweep WaveTriangle 440 880 0.35 0.22)
       in if psLevelIndex ps + 1 < length levels
            then (startLevel (psEpoch ps + 1) (psLevelIndex ps + 1), [fanfare])
            else (ps {psWon = True, psHeldKeys = Set.empty}, [fanfare])
  | Just (pid, actor) <- matchPair asPlate asWeightActor a b =
      let wasPressed = isPlatePressed ps pid
          nextMap = Map.insertWith Set.union pid (Set.singleton actor) (psPlateOccupants ps)
          sfx = [PlaySound (tone WaveTriangle 440 0.09 0.14) | not wasPressed]
       in (ps {psPlateOccupants = nextMap}, sfx)
  | Just (bid, ()) <- matchPair asButtonZone (only Player) a b =
      (ps {psNearbyButtons = Set.insert bid (psNearbyButtons ps)}, [])
  | otherwise = (ps, [])

handleCollisionEnd
  :: PuzzleBody
  -> PuzzleBody
  -> PuzzleState
  -> (PuzzleState, [Command PuzzleBody])
handleCollisionEnd a b ps
  | Just (pid, actor) <- matchPair asPlate asWeightActor a b =
      let nextMap = Map.adjust (Set.delete actor) pid (psPlateOccupants ps)
          ps' = ps {psPlateOccupants = nextMap}
          released = isPlatePressed ps pid && not (isPlatePressed ps' pid)
          sfx = [PlaySound (tone WaveTriangle 310 0.08 0.10) | released]
       in (ps', sfx)
  | Just (bid, ()) <- matchPair asButtonZone (only Player) a b =
      (ps {psNearbyButtons = Set.delete bid (psNearbyButtons ps)}, [])
  | otherwise = (ps, [])

-- | Pure render function: transforms PuzzleState into a declarative 3D Scene.
renderPuzzle :: PuzzleState -> Scene PuzzleBody PuzzleTimer
renderPuzzle ps =
  let lvl = currentLevel ps
   in defaultScene
        { sceneEpoch = psEpoch ps
        , sceneGravity = Vec3 0 (-26) 0
        , sceneBackground = Color 0.10 0.13 0.18
        , sceneCamera =
            FollowCamera
              { camTarget = Player
              , camOffset = Vec3 0 15.5 11.5
              , camLookOffset = Vec3 0 0.4 (-1.2)
              , camLocal = False
              , camSmooth = 6.5
              , camFov = 58
              }
        , sceneLights =
            [ HemisphereLight (Color 0.80 0.88 1.0) (Color 0.22 0.24 0.30) 0.72
            , DirectionalLight white 1.15 (Vec3 14 28 16)
            , PointLight (Color 0.55 0.78 1.0) 1.4 (lvlExitPos lvl + Vec3 0 3.0 0) 14
            ]
        , sceneTree =
            (levelStaticTrees !! psLevelIndex ps)
              <> (levelExitTrees !! psLevelIndex ps)
              <> (levelCrateTrees !! psLevelIndex ps)
              <> bodyNode (playerBody ps lvl)
              <> bodies (map (plateBody ps) (lvlPlates lvl))
              <> bodies (concatMap (buttonBodies ps) (lvlButtons lvl))
              <> bodies (map (doorBody ps) (lvlDoors lvl))
        , sceneTimers =
            [ Once (ButtonTimeout bid nonce) secs
            | (bid, (nonce, secs)) <- Map.toList (psButtonTimers ps)
            ]
        , sceneHud = buildHud ps lvl
        }

floorBody :: Double -> Double -> Body PuzzleBody
floorBody fw fd =
  staticBody
    Floor
    (at (Vec3 0 (-0.5) 0))
    (BoxCollider (Vec3 fw 1.0 fd))
    (colored (Color 0.23 0.27 0.33) (box (Vec3 fw 1.0 fd)))

wallBody :: WallDef -> Body PuzzleBody
wallBody w =
  staticBody
    (Wall (wallNum w))
    (at (wallPos w))
    (BoxCollider (wallSize w))
    (colored slate (box (wallSize w)))

playerBody :: PuzzleState -> LevelDef -> Body PuzzleBody
playerBody ps lvl =
  let mesh =
        group
          [ colored blue $ cylinder 0.40 0.44 1.15
          , colored (Color 0.95 0.82 0.70) $ translate (Vec3 0 0.85 0) $ sphere 0.34
          , glow cyan 0.7 $ colored cyan $ translate (Vec3 0 0.88 (-0.25)) $ box (Vec3 0.42 0.14 0.18)
          ]
      base =
        dynamicBody
          Player
          (at (lvlPlayerStart lvl))
          (CapsuleCollider 0.45 0.8)
          mesh
   in base
        { bodyMass = 2.0
        , bodyFriction = 0.0
        , bodyRestitution = 0.0
        , bodyLockRot = True
        , bodyDrive =
            if psWon ps
              then WalkDrive vZero
              else computeWalkDrive (psHeldKeys ps)
        }

computeWalkDrive :: Set Key -> Drive
computeWalkDrive keys =
  let held k = Set.member k keys
      up = held (KeyChar 'w') || held KeyUpArrow
      down = held (KeyChar 's') || held KeyDownArrow
      left = held (KeyChar 'a') || held KeyLeftArrow
      right = held (KeyChar 'd') || held KeyRightArrow
      dx = (if right then 1 else 0) + (if left then (-1) else 0)
      dz = (if down then 1 else 0) + (if up then (-1) else 0)
      dir = vNorm (Vec3 dx 0 dz)
   in WalkDrive (7.6 *^ dir)

crateBody :: CrateDef -> Body PuzzleBody
crateBody c =
  let mesh =
        group
          [ colored (Color 0.72 0.48 0.22) $ cube 1.38
          , colored (Color 0.48 0.30 0.14) $ box (Vec3 1.44 0.20 1.44)
          , colored (Color 0.48 0.30 0.14) $ box (Vec3 0.20 1.44 1.44)
          ]
      base =
        dynamicBody
          (Crate (crateNum c))
          (at (cratePos c))
          (BoxCollider (Vec3 1.38 1.38 1.38))
          mesh
   in base
        { bodyMass = 1.5
        , bodyFriction = 0.15
        , bodyRestitution = 0.0
        , bodyDamping = (6.5, 6.5)
        , bodyLockRot = True
        }

plateBody :: PuzzleState -> PlateDef -> Body PuzzleBody
plateBody ps p =
  let active = isPlatePressed ps (plateId p)
      col = if active then green else orange
      mesh =
        group
          [ colored darkGray $ box (Vec3 2.0 0.10 2.0)
          , glow col (if active then 1.4 else 0.5) $
              colored col $
                translate (Vec3 0 0.07 0) $
                  box (Vec3 1.65 0.08 1.65)
          ]
   in sensorBody
        (Plate (plateId p))
        (at (platePos p))
        (BoxCollider (Vec3 1.75 0.65 1.75))
        mesh

buttonBodies :: PuzzleState -> ButtonDef -> [Body PuzzleBody]
buttonBodies ps b =
  let active = Set.member (btnId b) (psActiveButtons ps)
      col = case btnMode b of
        ToggleButton -> if active then cyan else red
        TimedButton _ -> if active then gold else red
      Vec3 bx _ bz = btnPos b
      pedestalMesh =
        group
          [ colored darkGray $ translate (Vec3 0 0.55 0) $ cylinder 0.32 0.38 1.1
          , glow col (if active then 1.6 else 0.6) $
              colored col $
                translate (Vec3 0 1.16 0) $
                  cylinder 0.26 0.26 0.18
          ]
      pedestal =
        staticBody
          (Button (btnId b))
          (at (Vec3 bx 0 bz))
          (CylinderCollider 0.38 1.2)
          pedestalMesh
      zone =
        sensorBody
          (ButtonZone (btnId b))
          (at (Vec3 bx 1.0 bz))
          (BoxCollider (Vec3 2.5 2.0 2.5))
          emptyMesh
   in [pedestal, zone]

doorBody :: PuzzleState -> DoorDef -> Body PuzzleBody
doorBody ps d =
  let open = isDoorOpen ps d
      Vec3 dx dy dz = doorClosedPos d
      Vec3 sx sy sz = doorSize d
      targetPos = if open then Vec3 dx (dy + sy + 0.25) dz else Vec3 dx dy dz
      accent = if open then green else red
      mesh =
        group
          [ colored (Color 0.42 0.47 0.54) $ box (Vec3 sx sy sz)
          , glow accent 1.2 $
              colored accent $
                translate (Vec3 0 (-sy * 0.38) 0) $
                  box (Vec3 (sx + 0.04) 0.22 (sz + 0.04))
          ]
   in kinematicBody (Door (doorId d)) 6.0 (at targetPos) (BoxCollider (doorSize d)) mesh

exitBody :: Vec3 -> Body PuzzleBody
exitBody pos =
  let mesh =
        group
          [ colored darkGray $ cylinder 1.35 1.45 0.14
          , glow purple 1.8 $ colored cyan $ translate (Vec3 0 0.12 0) $ cylinder 1.0 1.0 0.14
          , glow cyan 1.2 $ colored cyan $ translate (Vec3 0 1.2 0) $ sphere 0.38
          ]
   in sensorBody Exit (at pos) (BoxCollider (Vec3 2.0 2.4 2.0)) mesh

buildHud :: PuzzleState -> LevelDef -> [HudElement]
buildHud ps lvl =
  let header =
        HudText TopLeft $
          "Level "
            <> T.pack (show (lvlNumber lvl))
            <> "/"
            <> T.pack (show (length levels))
            <> ": "
            <> lvlName lvl
      hint = HudText TopCenter (lvlHint lvl)
      controls =
        HudText BottomCenter $
          "WASD / Arrows: Walk & Push Crates   ·   E or Click: Press Button   ·   R: Reset Level   ·   1/2/3: Select Level"
      prompt =
        case Set.toList (psNearbyButtons ps) of
          [] -> []
          (bid : _) ->
            let lbl =
                  case filter (\b -> btnId b == bid) (lvlButtons lvl) of
                    (b : _) -> btnLabel b
                    [] -> T.pack (show bid)
             in [HudText TopRight ("Press [E] or Click — " <> lbl)]
      wonBanner =
        [ HudBanner
            "ALL LEVELS CLEARED!"
            "You mastered switches, weighted plates, timed doors, and airlocks.\nPress [R] to play again from Level 1."
        | psWon ps
        ]
   in [header, hint, controls] <> prompt <> wonBanner
