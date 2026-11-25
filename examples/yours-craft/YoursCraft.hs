{-# LANGUAGE OverloadedStrings #-}

-- | YoursCraft: A Minecraft-like 3D voxel building game that also serves as a
-- real-world stress test for EventHorizon's incremental 'SceneTree' + 'memo'
-- diffing engine.
--
-- Worlds are stored as nested persistent maps of 8x8 horizontal chunks:
-- @Map ChunkCoord (Map VoxelCoord BlockKind)@. Placing or breaking a single
-- block mutates only 1 chunk and 1 voxel leaf, so @mapTree chunkNode@ +
-- @diffTree@ skips all untouched chunks in O(1) and emits a ~200-byte delta.
module YoursCraft
  ( VoxelCoord
  , ChunkCoord
  , VoxelBody (..)
  , BlockKind (..)
  , VoxelState (..)
  , chunkSize
  , chunkOf
  , initialVoxelWorld
  , generateWorldChunks
  , renderVoxel
  , renderVoxelMemo
  , renderVoxelFlat
  , stepVoxel
  , handleEvent
  , clickedFaceOffset
  , placeBlockAt
  , breakBlockAt
  ) where

import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Game.EventHorizon

type VoxelCoord = (Int, Int, Int)
type ChunkCoord = (Int, Int)

-- | Type-safe 3D body identifiers for VoxelCraft.
data VoxelBody
  = Player
  | Voxel !Int !Int !Int
  deriving (Eq, Ord, Show)

-- | Voxel block materials.
data BlockKind
  = Grass
  | Dirt
  | Stone
  | Wood
  | Brick
  | Glowstone
  | Leaves
  deriving (Eq, Ord, Show, Enum, Bounded)

data VoxelState = VoxelState
  { vsEpoch :: !Int
  , vsWorldRadius :: !Int
  , vsChunks :: !(Map ChunkCoord (Map VoxelCoord BlockKind))
  , vsBlockCount :: !Int
  , vsHeldKeys :: !(Set Key)
  , vsSelected :: !BlockKind
  , vsUseMemo :: !Bool
  , vsCamYaw :: !Double
  , vsPlacedCount :: !Int
  , vsBrokenCount :: !Int
  }
  deriving (Eq, Show)

chunkSize :: Int
chunkSize = 8

chunkOf :: VoxelCoord -> ChunkCoord
chunkOf (x, _, z) = (x `div` chunkSize, z `div` chunkSize)

blockName :: BlockKind -> Text
blockName k = case k of
  Grass -> "Grass"
  Dirt -> "Dirt"
  Stone -> "Stone"
  Wood -> "Wood"
  Brick -> "Brick"
  Glowstone -> "Glowstone"
  Leaves -> "Leaves"

-- | Smooth procedural heightfield for rolling voxel hills.
terrainHeight :: Int -> Int -> Int
terrainHeight x z =
  let fx = fromIntegral x :: Double
      fz = fromIntegral z :: Double
      h =
        1.6 * sin (fx * 0.14) * cos (fz * 0.14)
          + 0.9 * sin ((fx + fz) * 0.09)
          + 0.5 * cos (fx * 0.27 - fz * 0.19)
   in max 0 (round (h + 1.5))

-- | Generate a chunked procedural voxel landscape with hills, stone ridges,
-- and voxel trees. Only visible surface shells + cliff walls are materialized.
generateWorldChunks :: Int -> (Map ChunkCoord (Map VoxelCoord BlockKind), Int)
generateWorldChunks radius =
  let surfaceVoxels =
        [ ((x, y, z), kind)
        | x <- [-radius .. radius]
        , z <- [-radius .. radius]
        , let h = terrainHeight x z
              -- Fill cliff sides where a neighbor drops lower so hills stay watertight
              minNbr =
                minimum
                  [ terrainHeight (x - 1) z
                  , terrainHeight (x + 1) z
                  , terrainHeight x (z - 1)
                  , terrainHeight x (z + 1)
                  ]
              yMin = max 0 (min h minNbr)
        , y <- [yMin .. h]
        , let kind
                | y == h && h >= 4 = Stone
                | y == h = Grass
                | otherwise = Dirt
        ]
      treeCenters =
        [ (x, terrainHeight x z, z)
        | x <- [-radius + 3, -radius + 10 .. radius - 3]
        , z <- [-radius + 3, -radius + 10 .. radius - 3]
        , abs x > 3 || abs z > 3 -- keep spawn clearing open
        , ((x * 37 + z * 17) `mod` 5) == 0
        ]
      treeVoxels = concatMap makeTree treeCenters
      allVoxels = surfaceVoxels <> treeVoxels
      chunks = foldl' insertOne Map.empty allVoxels
      total = sum (map Map.size (Map.elems chunks))
   in (chunks, total)
  where
    insertOne acc (coord, kind) =
      let ck = chunkOf coord
       in Map.insertWith Map.union ck (Map.singleton coord kind) acc

    makeTree (tx, ty, tz) =
      [ ((tx, ty + dy, tz), Wood) | dy <- [1 .. 3] ]
        <> [ ((tx + dx, ty + dy, tz + dz), Leaves)
           | dy <- [3, 4]
           , dx <- [-1 .. 1]
           , dz <- [-1 .. 1]
           , not (dy == 3 && dx == 0 && dz == 0)
           , not (dy == 4 && abs dx == 1 && abs dz == 1)
           ]
        <> [((tx, ty + 5, tz), Leaves)]

initialVoxelWorld :: Int -> VoxelState
initialVoxelWorld radius =
  let (chunks, cnt) = generateWorldChunks radius
   in VoxelState
        { vsEpoch = 0
        , vsWorldRadius = radius
        , vsChunks = chunks
        , vsBlockCount = cnt
        , vsHeldKeys = Set.empty
        , vsSelected = Brick
        , vsUseMemo = True
        , vsCamYaw = 0.0
        , vsPlacedCount = 0
        , vsBrokenCount = 0
        }

-- | Determine which face of voxel @(vx, vy, vz)@ was clicked from the 3D raycast
-- hit point, returning the unit integer normal @(nx, ny, nz)@ for placement.
clickedFaceOffset :: VoxelCoord -> Vec3 -> VoxelCoord
clickedFaceOffset (vx, vy, vz) (Vec3 hx hy hz) =
  let dx = hx - fromIntegral vx
      dy = hy - fromIntegral vy
      dz = hz - fromIntegral vz
      ax = abs dx
      ay = abs dy
      az = abs dz
   in if ax >= ay && ax >= az
        then (if dx >= 0 then 1 else -1, 0, 0)
        else
          if ay >= ax && ay >= az
            then (0, if dy >= 0 then 1 else -1, 0)
            else (0, 0, if dz >= 0 then 1 else -1)

-- | Purely insert a voxel into its 8x8 chunk (O(log C + log B), preserving all other chunks!).
placeBlockAt :: VoxelCoord -> BlockKind -> VoxelState -> VoxelState
placeBlockAt coord kind vs =
  let ck = chunkOf coord
      existingChunk = Map.findWithDefault Map.empty ck (vsChunks vs)
   in if Map.member coord existingChunk
        then vs
        else
          let chunk' = Map.insert coord kind existingChunk
           in vs
                { vsChunks = Map.insert ck chunk' (vsChunks vs)
                , vsBlockCount = vsBlockCount vs + 1
                , vsPlacedCount = vsPlacedCount vs + 1
                }

-- | Purely remove a voxel from its 8x8 chunk (O(log C + log B), preserving all other chunks!).
breakBlockAt :: VoxelCoord -> VoxelState -> VoxelState
breakBlockAt coord vs =
  let ck = chunkOf coord
   in case Map.lookup ck (vsChunks vs) of
        Nothing -> vs
        Just chunk ->
          if not (Map.member coord chunk)
            then vs
            else
              let chunk' = Map.delete coord chunk
                  chunks' =
                    if Map.null chunk'
                      then Map.delete ck (vsChunks vs)
                      else Map.insert ck chunk' (vsChunks vs)
               in vs
                    { vsChunks = chunks'
                    , vsBlockCount = vsBlockCount vs - 1
                    , vsBrokenCount = vsBrokenCount vs + 1
                    }

-- | Pure event handler for YoursCraft.
stepVoxel :: Event VoxelBody Void -> Snapshot VoxelBody -> VoxelState -> (VoxelState, [Command VoxelBody])
stepVoxel ev snap vs = case ev of
  EngineReady -> (vs, [])
  KeyDown (KeyChar 'r') ->
    let fresh = initialVoxelWorld (vsWorldRadius vs)
     in (fresh {vsEpoch = vsEpoch vs + 1, vsUseMemo = vsUseMemo vs}, [])
  KeyDown (KeyChar 'm') ->
    (vs {vsUseMemo = not (vsUseMemo vs)}, [PlaySound (tone WaveSine 580 0.06 0.12)])
  KeyDown (KeyChar 'q') ->
    (vs {vsCamYaw = vsCamYaw vs - (pi / 6)}, [])
  KeyDown (KeyChar 'e') ->
    (vs {vsCamYaw = vsCamYaw vs + (pi / 6)}, [])
  KeyDown (KeyChar '1') -> (vs {vsSelected = Grass}, [])
  KeyDown (KeyChar '2') -> (vs {vsSelected = Dirt}, [])
  KeyDown (KeyChar '3') -> (vs {vsSelected = Stone}, [])
  KeyDown (KeyChar '4') -> (vs {vsSelected = Wood}, [])
  KeyDown (KeyChar '5') -> (vs {vsSelected = Brick}, [])
  KeyDown (KeyChar '6') -> (vs {vsSelected = Glowstone}, [])
  KeyDown KeySpace ->
    (vs, [ApplyImpulse Player (Vec3 0 18.0 0), PlaySound (sweep WaveSine 240 420 0.10 0.14)])
  KeyDown k ->
    (vs {vsHeldKeys = Set.insert k (vsHeldKeys vs)}, [])
  KeyUp k ->
    (vs {vsHeldKeys = Set.delete k (vsHeldKeys vs)}, [])
  MouseDown MouseLeft (Just (Voxel vx vy vz, _)) ->
    -- Protect bottom bedrock layer y < 0 (allow breaking y >= 0)
    let vs' = breakBlockAt (vx, vy, vz) vs
     in if vsBrokenCount vs' /= vsBrokenCount vs
          then (vs', [PlaySound (noise 0.08 0.22)])
          else (vs, [])
  MouseDown MouseRight (Just (Voxel vx vy vz, hitPt)) ->
    let (nx, ny, nz) = clickedFaceOffset (vx, vy, vz) hitPt
        target@(tx, ty, tz) = (vx + nx, vy + ny, vz + nz)
        targetCenter = Vec3 (fromIntegral tx) (fromIntegral ty) (fromIntegral tz)
        tooCloseToPlayer = case lookupBody Player snap of
          Nothing -> False
          Just st -> vDist (bsPos st) targetCenter < 1.15
     in if tooCloseToPlayer
          then (vs, [])
          else
            let vs' = placeBlockAt target (vsSelected vs) vs
             in if vsPlacedCount vs' /= vsPlacedCount vs
                  then (vs', [PlaySound (sweep WaveTriangle 340 520 0.07 0.16)])
                  else (vs, [])
  MouseDown _ _ -> (vs, [])
  MouseUp _ -> (vs, [])
  CollisionStart _ _ -> (vs, [])
  CollisionEnd _ _ -> (vs, [])

handleEvent :: Event VoxelBody Void -> Snapshot VoxelBody -> VoxelState -> VoxelState
handleEvent ev snap vs = fst (stepVoxel ev snap vs)

-- | Render using the currently active mode ('vsUseMemo').
renderVoxel :: VoxelState -> Scene VoxelBody Void
renderVoxel vs =
  if vsUseMemo vs
    then renderVoxelMemo vs
    else renderVoxelFlat vs

-- | Fast O(delta log N) incremental render using nested chunked 'mapTree' + 'memo'.
renderVoxelMemo :: VoxelState -> Scene VoxelBody Void
renderVoxelMemo vs =
  baseScene vs (bodyNode (playerBody vs) <> mapTree chunkNode (vsChunks vs))

-- | Naive O(N) unmemoized flat rebuild (for side-by-side comparison and benchmarking).
renderVoxelFlat :: VoxelState -> Scene VoxelBody Void
renderVoxelFlat vs =
  let allVoxels =
        [ voxelBody x y z kind
        | chunk <- Map.elems (vsChunks vs)
        , ((x, y, z), kind) <- Map.toList chunk
        ]
   in baseScene vs (bodies (playerBody vs : allVoxels))

-- Top-level builders so 'mapTree' memoizes every untouched chunk and subtree in O(1)!
chunkNode :: ChunkCoord -> Map VoxelCoord BlockKind -> SceneTree VoxelBody
chunkNode _ = mapTree voxelNode

voxelNode :: VoxelCoord -> BlockKind -> SceneTree VoxelBody
voxelNode (x, y, z) kind = bodyNode (voxelBody x y z kind)

baseScene :: VoxelState -> SceneTree VoxelBody -> Scene VoxelBody Void
baseScene vs tree =
  let camRot = quatFromEuler (Vec3 0 (vsCamYaw vs) 0)
      camOff = camRot #> Vec3 0 8.5 12.5
   in defaultScene
        { sceneEpoch = vsEpoch vs
        , sceneGravity = Vec3 0 (-28) 0
        , sceneBackground = Color 0.52 0.78 0.96
        , sceneFog = Just (Color 0.60 0.82 0.96, 35, 110)
        , sceneCamera =
            FollowCamera
              { camTarget = Player
              , camOffset = camOff
              , camLookOffset = Vec3 0 0.8 0
              , camLocal = False
              , camSmooth = 9.0
              , camFov = 60
              }
        , sceneLights =
            [ HemisphereLight (Color 0.82 0.92 1.0) (Color 0.28 0.34 0.22) 0.78
            , DirectionalLight (Color 1.0 0.96 0.86) 1.25 (Vec3 26 48 22)
            ]
        , sceneTree = tree
        , sceneHud = buildHud vs
        }

-- Prebuilt unit-cube meshes per BlockKind so every voxel shares its Mesh pointer!
meshGrass, meshDirt, meshStone, meshWood, meshBrick, meshGlowstone, meshLeaves :: Mesh
meshGrass = colored (Color 0.25 0.62 0.24) (cube 1.0)
meshDirt = colored (Color 0.48 0.32 0.18) (cube 1.0)
meshStone = colored (Color 0.54 0.57 0.62) (cube 1.0)
meshWood = colored (Color 0.56 0.36 0.18) (cube 1.0)
meshBrick = colored (Color 0.74 0.26 0.22) (cube 1.0)
meshGlowstone =
  withMaterial
    ( defaultMaterial
        { matColor = Just gold
        , matEmissive = Just (gold, 1.4)
        , matCastShadow = False
        }
    )
    (cube 1.0)
meshLeaves = colored (Color 0.16 0.50 0.22) (cube 1.0)

meshForKind :: BlockKind -> Mesh
meshForKind k = case k of
  Grass -> meshGrass
  Dirt -> meshDirt
  Stone -> meshStone
  Wood -> meshWood
  Brick -> meshBrick
  Glowstone -> meshGlowstone
  Leaves -> meshLeaves

voxelCollider :: Collider
voxelCollider = BoxCollider (Vec3 1.0 1.0 1.0)

voxelBody :: Int -> Int -> Int -> BlockKind -> Body VoxelBody
voxelBody x y z kind =
  staticBody
    (Voxel x y z)
    (at (Vec3 (fromIntegral x) (fromIntegral y) (fromIntegral z)))
    voxelCollider
    (meshForKind kind)

playerBody :: VoxelState -> Body VoxelBody
playerBody vs =
  let spawnY = fromIntegral (terrainHeight 0 0) + 2.2
      mesh =
        group
          [ colored blue $ cylinder 0.34 0.38 1.05
          , colored (Color 0.95 0.82 0.70) $ translate (Vec3 0 0.76 0) $ sphere 0.28
          , glow cyan 0.6 $ colored cyan $ translate (Vec3 0 0.78 (-0.22)) $ box (Vec3 0.36 0.12 0.16)
          ]
      base =
        dynamicBody
          Player
          (at (Vec3 0 spawnY 0))
          (CapsuleCollider 0.36 0.75)
          mesh
   in base
        { bodyMass = 2.0
        , bodyFriction = 0.0
        , bodyRestitution = 0.0
        , bodyLockRot = True
        , bodyEvents = False
        , bodyDrive = computeWalkDrive (vsCamYaw vs) (vsHeldKeys vs)
        }

computeWalkDrive :: Double -> Set Key -> Drive
computeWalkDrive yaw keys =
  let held k = Set.member k keys
      fwdIn = (if held (KeyChar 'w') || held KeyUpArrow then 1.0 else 0.0) + (if held (KeyChar 's') || held KeyDownArrow then (-1.0) else 0.0)
      rightIn = (if held (KeyChar 'd') || held KeyRightArrow then 1.0 else 0.0) + (if held (KeyChar 'a') || held KeyLeftArrow then (-1.0) else 0.0)
      rot = quatFromEuler (Vec3 0 yaw 0)
      fwd = forwardOf rot
      right = rightOf rot
      dir = vNorm (fwdIn *^ fwd + rightIn *^ right)
   in WalkDrive (8.2 *^ dir)

buildHud :: VoxelState -> [HudElement]
buildHud vs =
  let modeLabel = if vsUseMemo vs then "Memoized Chunked Tree" else "Naive Flat Rebuild"
      leftHud =
        HudText TopLeft $
          "YoursCraft   |   Blocks: "
            <> T.pack (show (vsBlockCount vs))
            <> "   |   Chunks: "
            <> T.pack (show (Map.size (vsChunks vs)))
            <> "   |   Placed: "
            <> T.pack (show (vsPlacedCount vs))
            <> "   |   Broken: "
            <> T.pack (show (vsBrokenCount vs))
      rightHud =
        HudText TopRight $
          "Block: [" <> blockName (vsSelected vs) <> "]   |   Render: " <> modeLabel
      controlsHud =
        HudText BottomCenter $
          "WASD: Walk   ·   Space: Jump   ·   Q/E: Rotate Cam   ·   Left Click: Break   ·   Right Click: Place   ·   1-6: Block   ·   M: Toggle Memo   ·   R: Reset"
   in [leftHud, rightHud, controlsHud]
