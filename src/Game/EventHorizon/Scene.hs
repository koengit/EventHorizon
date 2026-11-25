module Game.EventHorizon.Scene
  ( -- * Materials & 3D Visuals
    Material (..)
  , defaultMaterial
  , Mesh (..)
  , emptyMesh
  , box
  , cube
  , sphere
  , cylinder
  , cone
  , capsule
  , torus
  , plane
  , colored
  , glow
  , withMaterial
  , translate
  , rotate
  , scale
  , scaleUniform
  , group

    -- * Physics Bodies & Colliders
  , Collider (..)
  , CollisionFilter (..)
  , defaultCollisionFilter
  , BodyKind (..)
  , Drive (..)
  , Body (..)
  , defaultBody
  , staticBody
  , dynamicBody
  , kinematicBody
  , sensorBody

    -- * Camera, Lights, Timers, HUD
  , Camera (..)
  , fixedCamera
  , followCamera
  , Light (..)
  , Timer (..)
  , HudAnchor (..)
  , HudElement (..)

    -- * Scene Tree & Incremental Collections
  , SceneTree (..)
  , bodyNode
  , bodies
  , mapTree
  , mapTreeWith
  , intMapTree
  , intMapTreeWith
  , treeBodies

    -- * Full Scene
  , Scene (..)
  , defaultScene
  ) where

import qualified Data.IntMap.Internal as IMI
import Data.IntMap.Strict (IntMap)
import qualified Data.Map.Internal as MI
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Word (Word16)
import Game.EventHorizon.Math
import Game.EventHorizon.Memo (memo2, memo3)

-- | Surface shading properties applied to child meshes.
data Material = Material
  { matColor :: !(Maybe Color)
  , matEmissive :: !(Maybe (Color, Double))
  , matRoughness :: !Double
  , matMetalness :: !Double
  , matOpacity :: !Double
  , matWireframe :: !Bool
  , matCastShadow :: !Bool
  , matReceiveShadow :: !Bool
  }
  deriving (Eq, Show)

defaultMaterial :: Material
defaultMaterial =
  Material
    { matColor = Nothing
    , matEmissive = Nothing
    , matRoughness = 0.62
    , matMetalness = 0.08
    , matOpacity = 1.0
    , matWireframe = False
    , matCastShadow = True
    , matReceiveShadow = True
    }

-- | Pure declarative 3D visual tree.
-- Forms a 'Semigroup' and 'Monoid' under grouping ('<>' and 'mempty').
data Mesh
  = EmptyMesh
  | BoxMesh !Vec3
  | SphereMesh !Double
  | CylinderMesh !Double !Double !Double
  | ConeMesh !Double !Double
  | CapsuleMesh !Double !Double
  | TorusMesh !Double !Double
  | PlaneMesh !Double !Double
  | ColorMesh !Color !Mesh
  | EmissiveMesh !Color !Double !Mesh
  | MaterialMesh !Material !Mesh
  | TranslateMesh !Vec3 !Mesh
  | RotateMesh !Vec3 !Mesh
  | ScaleMesh !Vec3 !Mesh
  | GroupMesh ![Mesh]
  deriving (Eq, Show)

instance Semigroup Mesh where
  EmptyMesh <> b = b
  a <> EmptyMesh = a
  GroupMesh as <> GroupMesh bs = GroupMesh (as <> bs)
  GroupMesh as <> b = GroupMesh (as <> [b])
  a <> GroupMesh bs = GroupMesh (a : bs)
  a <> b = GroupMesh [a, b]

instance Monoid Mesh where
  mempty = EmptyMesh
  mconcat [] = EmptyMesh
  mconcat [x] = x
  mconcat xs = GroupMesh (concatMap flatten xs)
    where
      flatten EmptyMesh = []
      flatten (GroupMesh ys) = ys
      flatten y = [y]

emptyMesh :: Mesh
emptyMesh = EmptyMesh

box :: Vec3 -> Mesh
box = BoxMesh

cube :: Double -> Mesh
cube s = BoxMesh (Vec3 s s s)

sphere :: Double -> Mesh
sphere = SphereMesh

cylinder :: Double -> Double -> Double -> Mesh
cylinder = CylinderMesh

cone :: Double -> Double -> Mesh
cone = ConeMesh

capsule :: Double -> Double -> Mesh
capsule = CapsuleMesh

torus :: Double -> Double -> Mesh
torus = TorusMesh

plane :: Double -> Double -> Mesh
plane = PlaneMesh

colored :: Color -> Mesh -> Mesh
colored = ColorMesh

glow :: Color -> Double -> Mesh -> Mesh
glow = EmissiveMesh

withMaterial :: Material -> Mesh -> Mesh
withMaterial = MaterialMesh

translate :: Vec3 -> Mesh -> Mesh
translate = TranslateMesh

rotate :: Vec3 -> Mesh -> Mesh
rotate = RotateMesh

scale :: Vec3 -> Mesh -> Mesh
scale = ScaleMesh

scaleUniform :: Double -> Mesh -> Mesh
scaleUniform s = ScaleMesh (Vec3 s s s)

group :: [Mesh] -> Mesh
group = mconcat

-- | Collision shape attached to a physics body.
data Collider
  = NoCollider
  | BoxCollider !Vec3
  | SphereCollider !Double
  | CylinderCollider !Double !Double
  | CapsuleCollider !Double !Double
  | -- | Multiple colliders attached to a single rigid body at local offsets.
    CompoundCollider ![(Pose, Collider)]
  deriving (Eq, Show)

-- | 16-bit membership and filter bitmasks mapped directly to Rapier3D collision groups.
-- Two bodies collide iff @(groupA .&. maskB /= 0) && (groupB .&. maskA /= 0)@.
data CollisionFilter = CollisionFilter
  { cfGroup :: !Word16
  , cfMask :: !Word16
  }
  deriving (Eq, Show)

defaultCollisionFilter :: CollisionFilter
defaultCollisionFilter = CollisionFilter 0xFFFF 0xFFFF

-- | How a body participates in the physics simulation.
data BodyKind
  = Static
  | Dynamic
  | Kinematic !Double
  | Sensor
  deriving (Eq, Show)

-- | Continuous physics controller executed at 60 Hz inside the physics engine
-- without waking Haskell.
data Drive
  = NoDrive
  | -- | Horizontal walking velocity @(vx, 0, vz)@ preserving vertical gravity.
    WalkDrive !Vec3
  | -- | Direct world-space linear and angular velocity.
    VelocityDrive !Vec3 !Vec3
  | -- | Continuous world-space force and torque (respects mass, friction, and knockback).
    ForceDrive !Vec3 !Vec3
  | -- | Smooth steering toward a target world position.
    SeekDrive
      { seekTarget :: !Vec3
      , seekSpeed :: !Double
      , seekMaxForce :: !Double
      }
  | -- | Body-local arcade/sim flight controller.
    FlightDrive
      { flightSpeed :: !Double
      , flightPitch :: !Double
      , flightYaw :: !Double
      , flightRoll :: !Double
      , flightAutoLevel :: !Bool
      }
  deriving (Eq, Show)

-- | A 3D entity with a user-typed identifier @bid@, physics properties, and visual 'Mesh'.
data Body bid = Body
  { bodyId :: !bid
  , bodyKind :: !BodyKind
  , bodyPose :: !Pose
  , bodyCollider :: !Collider
  , bodyMesh :: !Mesh
  , bodyInitialVel :: !Vec3
  , bodyMass :: !Double
  , bodyFriction :: !Double
  , bodyRestitution :: !Double
  , bodyDamping :: !(Double, Double)
  , bodyLockRot :: !Bool
  , bodyGravity :: !Double
  , bodyCCD :: !Bool
  , bodyDrive :: !Drive
  , bodyFilter :: !CollisionFilter
  , -- | Whether this collider requests collision events from the physics engine.
    -- Note: Rapier emits an event if *either* colliding body has @bodyEvents = True@.
    bodyEvents :: !Bool
  }
  deriving (Eq, Show)

defaultBody :: bid -> BodyKind -> Pose -> Collider -> Mesh -> Body bid
defaultBody bid kind pose col mesh =
  Body
    { bodyId = bid
    , bodyKind = kind
    , bodyPose = pose
    , bodyCollider = col
    , bodyMesh = mesh
    , bodyInitialVel = vZero
    , bodyMass = 1.0
    , bodyFriction = 0.5
    , bodyRestitution = 0.05
    , bodyDamping = (0.0, 0.0)
    , bodyLockRot = False
    , bodyGravity = 1.0
    , bodyCCD = False
    , bodyDrive = NoDrive
    , bodyFilter = defaultCollisionFilter
    , bodyEvents = case kind of
        Static -> False
        _ -> True
    }

staticBody :: bid -> Pose -> Collider -> Mesh -> Body bid
staticBody bid = defaultBody bid Static

dynamicBody :: bid -> Pose -> Collider -> Mesh -> Body bid
dynamicBody bid = defaultBody bid Dynamic

kinematicBody :: bid -> Double -> Pose -> Collider -> Mesh -> Body bid
kinematicBody bid speed = defaultBody bid (Kinematic speed)

sensorBody :: bid -> Pose -> Collider -> Mesh -> Body bid
sensorBody bid = defaultBody bid Sensor

-- | Scene camera configuration.
data Camera bid
  = FixedCamera
      { camPos :: !Vec3
      , camLookAt :: !Vec3
      , camFov :: !Double
      }
  | FollowCamera
      { camTarget :: !bid
      , camOffset :: !Vec3
      , camLookOffset :: !Vec3
      , camLocal :: !Bool
      , camSmooth :: !Double
      , camFov :: !Double
      }
  deriving (Eq, Show)

fixedCamera :: Vec3 -> Vec3 -> Camera bid
fixedCamera pos look = FixedCamera pos look 60

followCamera :: bid -> Vec3 -> Vec3 -> Camera bid
followCamera target off lookOff = FollowCamera target off lookOff False 7.0 60

-- | Scene lights.
data Light
  = AmbientLight !Color !Double
  | HemisphereLight !Color !Color !Double
  | DirectionalLight !Color !Double !Vec3
  | PointLight !Color !Double !Vec3 !Double
  deriving (Eq, Show)

-- | Declarative timers managed by the runtime.
-- Removing a timer from 'sceneTimers' cancels it.
data Timer tid
  = Once !tid !Double
  | Every !tid !Double
  deriving (Eq, Show)

data HudAnchor
  = TopLeft
  | TopCenter
  | TopRight
  | Center
  | BottomCenter
  deriving (Eq, Show)

data HudElement
  = HudText !HudAnchor !Text
  | HudCrosshair
  | HudBanner !Text !Text
  deriving (Eq, Show)

-- | Declarative binary tree of 3D bodies in a scene.
--
-- Forms a 'Semigroup' and 'Monoid' under composition ('<>' skips 'EmptyTree',
-- and 'mconcat' builds a balanced binary tree). Combining 'SceneTree' with
-- 'memo', 'mapTree', or 'intMapTree' allows unchanged subtrees to keep their
-- exact heap pointers across frames so 'Game.EventHorizon.Diff.diffTree' skips them
-- in O(1).
data SceneTree bid
  = EmptyTree
  | OneBody !(Body bid)
  | Branch !(SceneTree bid) !(SceneTree bid)
  deriving (Eq, Show)

instance Semigroup (SceneTree bid) where
  EmptyTree <> b = b
  a <> EmptyTree = a
  a <> b = Branch a b

instance Monoid (SceneTree bid) where
  mempty = EmptyTree
  mconcat = balancedTrees . filter (not . isEmptyTree)
    where
      isEmptyTree EmptyTree = True
      isEmptyTree _ = False

balancedTrees :: [SceneTree bid] -> SceneTree bid
balancedTrees [] = EmptyTree
balancedTrees [t] = t
balancedTrees ts = balancedTrees (pairUp ts)
  where
    pairUp [] = []
    pairUp [x] = [x]
    pairUp (x : y : rest) = (x <> y) : pairUp rest

-- | Lift a single 'Body' into a 'SceneTree'.
bodyNode :: Body bid -> SceneTree bid
bodyNode = OneBody

-- | Build a balanced 'SceneTree' from a list of bodies.
bodies :: [Body bid] -> SceneTree bid
bodies = balancedTrees . map OneBody

-- | Incrementally map a persistent 'Map' into a 'SceneTree' by walking its
-- internal tree structure with 'memo'. Untouched subtrees reuse their exact
-- previous heap pointer in O(1).
--
-- For best results, pass a top-level function for @f@ so its closure pointer
-- stays constant across frames. If rendering depends on extra state @env@, use
-- 'mapTreeWith'.
{-# NOINLINE mapTree #-}
mapTree :: (k -> v -> SceneTree bid) -> Map k v -> SceneTree bid
mapTree = memo2 mapTreeStep

mapTreeStep :: (k -> v -> SceneTree bid) -> Map k v -> SceneTree bid
mapTreeStep _ MI.Tip = EmptyTree
mapTreeStep f (MI.Bin _ k v l r) =
  memo2 f k v <> (mapTree f l <> mapTree f r)

-- | Like 'mapTree', passing an extra environment @env@ and memoizing each
-- subtree on @(f, env, subMap)@ via 'memo3'.
{-# NOINLINE mapTreeWith #-}
mapTreeWith :: (env -> k -> v -> SceneTree bid) -> env -> Map k v -> SceneTree bid
mapTreeWith = memo3 mapTreeWithStep

mapTreeWithStep :: (env -> k -> v -> SceneTree bid) -> env -> Map k v -> SceneTree bid
mapTreeWithStep _ _ MI.Tip = EmptyTree
mapTreeWithStep f env (MI.Bin _ k v l r) =
  f env k v <> (mapTreeWith f env l <> mapTreeWith f env r)

-- | Incrementally map a persistent 'IntMap' into a 'SceneTree' by walking its
-- canonical Patricia trie with 'memo2'. Because 'IntMap' never rotates on
-- insert/delete, every untouched subtree stays in place and skips in O(1).
{-# NOINLINE intMapTree #-}
intMapTree :: (Int -> v -> SceneTree bid) -> IntMap v -> SceneTree bid
intMapTree = memo2 intMapTreeStep

intMapTreeStep :: (Int -> v -> SceneTree bid) -> IntMap v -> SceneTree bid
intMapTreeStep _ IMI.Nil = EmptyTree
intMapTreeStep f (IMI.Tip k v) = f k v
intMapTreeStep f (IMI.Bin _ _ l r) = intMapTree f l <> intMapTree f r

-- | Like 'intMapTree', passing an extra environment @env@ and memoizing each
-- subtree on @(f, env, subIntMap)@ via 'memo3'.
{-# NOINLINE intMapTreeWith #-}
intMapTreeWith :: (env -> Int -> v -> SceneTree bid) -> env -> IntMap v -> SceneTree bid
intMapTreeWith = memo3 intMapTreeWithStep

intMapTreeWithStep :: (env -> Int -> v -> SceneTree bid) -> env -> IntMap v -> SceneTree bid
intMapTreeWithStep _ _ IMI.Nil = EmptyTree
intMapTreeWithStep f env (IMI.Tip k v) = f env k v
intMapTreeWithStep f env (IMI.Bin _ _ l r) = intMapTreeWith f env l <> intMapTreeWith f env r

-- | Flatten all bodies in a 'SceneTree' in left-to-right order in O(n) time.
treeBodies :: SceneTree bid -> [Body bid]
treeBodies t = go t []
  where
    go EmptyTree acc = acc
    go (OneBody b) acc = b : acc
    go (Branch l r) acc = go l (go r acc)

-- | Complete declarative description of the 3D world returned by the game's
-- pure render function.
data Scene bid tid = Scene
  { sceneEpoch :: !Int
  , sceneGravity :: !Vec3
  , sceneBackground :: !Color
  , sceneFog :: !(Maybe (Color, Double, Double))
  , sceneCamera :: !(Camera bid)
  , sceneLights :: ![Light]
  , -- | Declarative tree of all 3D bodies in the scene. Compose static
    -- subtrees, dynamic 'Map'/'IntMap' collections ('mapTree', 'intMapTree'),
    -- and individual bodies ('bodyNode', 'bodies') with '<>' / 'mconcat'.
    sceneTree :: !(SceneTree bid)
  , sceneTimers :: ![Timer tid]
  , sceneHud :: ![HudElement]
  }
  deriving (Eq, Show)

defaultScene :: Scene bid tid
defaultScene =
  Scene
    { sceneEpoch = 0
    , sceneGravity = Vec3 0 (-22) 0
    , sceneBackground = skyBlue
    , sceneFog = Nothing
    , sceneCamera = fixedCamera (Vec3 0 10 18) vZero
    , sceneLights =
        [ AmbientLight white 0.55
        , DirectionalLight white 1.15 (Vec3 25 45 20)
        ]
    , sceneTree = EmptyTree
    , sceneTimers = []
    , sceneHud = []
    }
