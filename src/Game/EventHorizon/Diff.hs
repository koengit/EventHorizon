{-# LANGUAGE MagicHash #-}

-- | Server-side incremental scene diffing with O(1) pointer-equality skipping.
--
-- Instead of rebuilding giant flat maps of every body on every event,
-- 'diffScene' compares the previous and next 'SceneTree' structurally.
-- Whenever two subtrees share the same heap pointer (via CAFs, 'memo',
-- 'mapTree', or 'intMapTree'), 'diffTree' skips the entire subtree in O(1).
-- Any structural mismatches (such as 'Data.Map.Strict.Map' BST rotations) are
-- reconciled cleanly over only the small O(delta) mismatched leaves, ensuring
-- zero spurious despawns or respawns.
module Game.EventHorizon.Diff
  ( BodyUpdate (..)
  , SceneDelta (..)
  , ScenePatch (..)
  , DiffState (..)
  , isEmptyDelta
  , diffScene
  , diffTree
  , allSceneBodies
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import GHC.Exts (isTrue#, reallyUnsafePtrEquality#)
import Game.EventHorizon.Math
import Game.EventHorizon.Scene

-- | Referentially transparent O(1) pointer-equality shortcut.
-- If True, the two immutable values are guaranteed equal; if False, callers
-- fall back to structural 'Eq'.
sameRef :: a -> a -> Bool
sameRef a b = isTrue# (reallyUnsafePtrEquality# a b)

eqFast :: Eq a => a -> a -> Bool
eqFast a b = sameRef a b || a == b

-- | Incremental update to an existing body whose structural physics identity
-- (body kind category, collider shape, filter, lockRot, mass, friction, etc.)
-- has not changed.
data BodyUpdate bid = BodyUpdate
  { buId :: !bid
  , buMesh :: !(Maybe Mesh)
  , buPose :: !(Maybe Pose)
  , buDrive :: !(Maybe Drive)
  , buKinematicSpeed :: !(Maybe Double)
  , buGravity :: !(Maybe Double)
  , buDamping :: !(Maybe (Double, Double))
  }
  deriving (Eq, Show)

data BodyDiff bid
  = BodyUnchanged
  | BodyRespawn !(Body bid)
  | BodyModified !(BodyUpdate bid)

-- | Incremental patch applied when 'sceneEpoch' is unchanged.
data SceneDelta bid tid = SceneDelta
  { sdEpoch :: !Int
  , sdGravity :: !(Maybe Vec3)
  , sdBackground :: !(Maybe Color)
  , sdFog :: !(Maybe (Maybe (Color, Double, Double)))
  , sdCamera :: !(Maybe (Camera bid))
  , sdLights :: !(Maybe [Light])
  , sdTimers :: !(Maybe [Timer tid])
  , sdHud :: !(Maybe [HudElement])
  , sdRemoved :: ![bid]
  , sdAdded :: ![Body bid]
  , sdUpdated :: ![BodyUpdate bid]
  }
  deriving (Eq, Show)

-- | Patch sent from Haskell to the 3D/Physics runtime.
data ScenePatch bid tid
  = PatchReset !(Scene bid tid)
  | PatchDelta !(SceneDelta bid tid)
  deriving (Eq, Show)

-- | Retained previous scene for O(delta log N) tree diffing.
data DiffState bid tid = DiffState
  { dsScene :: !(Scene bid tid)
  }

isEmptyDelta :: SceneDelta bid tid -> Bool
isEmptyDelta d =
  isNothing (sdGravity d)
    && isNothing (sdBackground d)
    && isNothing (sdFog d)
    && isNothing (sdCamera d)
    && isNothing (sdLights d)
    && isNothing (sdTimers d)
    && isNothing (sdHud d)
    && null (sdRemoved d)
    && null (sdAdded d)
    && null (sdUpdated d)

allSceneBodies :: Scene bid tid -> [Body bid]
allSceneBodies sc = treeBodies (sceneTree sc)

sameKindCategory :: BodyKind -> BodyKind -> Bool
sameKindCategory Static Static = True
sameKindCategory Dynamic Dynamic = True
sameKindCategory (Kinematic _) (Kinematic _) = True
sameKindCategory Sensor Sensor = True
sameKindCategory _ _ = False

diffBody :: Body bid -> Body bid -> BodyDiff bid
diffBody old new
  | sameRef old new = BodyUnchanged
  | needsRespawn = BodyRespawn new
  | isNoOp = BodyUnchanged
  | otherwise = BodyModified upd
  where
    needsRespawn =
      not (sameKindCategory (bodyKind old) (bodyKind new))
        || not (eqFast (bodyCollider old) (bodyCollider new))
        || bodyMass old /= bodyMass new
        || bodyFriction old /= bodyFriction new
        || bodyRestitution old /= bodyRestitution new
        || bodyLockRot old /= bodyLockRot new
        || bodyCCD old /= bodyCCD new
        || bodyFilter old /= bodyFilter new
        || bodyEvents old /= bodyEvents new

    mMesh =
      if eqFast (bodyMesh old) (bodyMesh new)
        then Nothing
        else Just (bodyMesh new)
    mPose =
      if bodyPose old == bodyPose new
        then Nothing
        else Just (bodyPose new)
    mDrive =
      if bodyDrive old == bodyDrive new
        then Nothing
        else Just (bodyDrive new)
    mKinSpeed = case (bodyKind old, bodyKind new) of
      (Kinematic s1, Kinematic s2) | s1 /= s2 -> Just s2
      _ -> Nothing
    mGrav =
      if bodyGravity old == bodyGravity new
        then Nothing
        else Just (bodyGravity new)
    mDamp =
      if bodyDamping old == bodyDamping new
        then Nothing
        else Just (bodyDamping new)

    isNoOp =
      isNothing mMesh
        && isNothing mPose
        && isNothing mDrive
        && isNothing mKinSpeed
        && isNothing mGrav
        && isNothing mDamp

    upd =
      BodyUpdate
        { buId = bodyId new
        , buMesh = mMesh
        , buPose = mPose
        , buDrive = mDrive
        , buKinematicSpeed = mKinSpeed
        , buGravity = mGrav
        , buDamping = mDamp
        }

data RawDiff bid = RawDiff
  { rdRemoved :: !(Map bid (Body bid))
  , rdAdded :: !(Map bid (Body bid))
  , rdRespawned :: ![Body bid]
  , rdUpdated :: ![BodyUpdate bid]
  }

emptyRawDiff :: RawDiff bid
emptyRawDiff = RawDiff Map.empty Map.empty [] []

collectRemoved :: Ord bid => SceneTree bid -> RawDiff bid -> RawDiff bid
collectRemoved tree !acc = acc {rdRemoved = go tree (rdRemoved acc)}
  where
    go EmptyTree !m = m
    go (OneBody b) !m = Map.insert (bodyId b) b m
    go (Branch l r) !m = go r (go l m)

collectAdded :: Ord bid => SceneTree bid -> RawDiff bid -> RawDiff bid
collectAdded tree !acc = acc {rdAdded = go tree (rdAdded acc)}
  where
    go EmptyTree !m = m
    go (OneBody b) !m = Map.insert (bodyId b) b m
    go (Branch l r) !m = go r (go l m)

walkDiff :: Ord bid => SceneTree bid -> SceneTree bid -> RawDiff bid -> RawDiff bid
walkDiff old new !acc
  | sameRef old new = acc
  | otherwise = case (old, new) of
      (EmptyTree, EmptyTree) -> acc
      (EmptyTree, t) -> collectAdded t acc
      (t, EmptyTree) -> collectRemoved t acc
      (OneBody b1, OneBody b2)
        | sameRef b1 b2 -> acc
        | bodyId b1 == bodyId b2 ->
            case diffBody b1 b2 of
              BodyUnchanged -> acc
              BodyRespawn b -> acc {rdRespawned = b : rdRespawned acc}
              BodyModified u -> acc {rdUpdated = u : rdUpdated acc}
        | otherwise ->
            acc
              { rdRemoved = Map.insert (bodyId b1) b1 (rdRemoved acc)
              , rdAdded = Map.insert (bodyId b2) b2 (rdAdded acc)
              }
      (Branch l1 r1, Branch l2 r2) ->
        let !acc1 = walkDiff l1 l2 acc
         in walkDiff r1 r2 acc1
      (tOld, tNew) ->
        collectAdded tNew (collectRemoved tOld acc)

-- | Incrementally diff two 'SceneTree's, skipping pointer-equal subtrees in O(1)
-- and reconciling any structural mismatches (e.g. BST rotations) over only the
-- small O(delta) mismatched leaves.
diffTree
  :: Ord bid
  => SceneTree bid
  -> SceneTree bid
  -> ([bid], [Body bid], [BodyUpdate bid])
diffTree oldTree newTree
  | sameRef oldTree newTree = ([], [], [])
  | otherwise =
      let raw = walkDiff oldTree newTree emptyRawDiff
          rawRem = rdRemoved raw
          rawAdd = rdAdded raw
          respawned = reverse (rdRespawned raw)
          updated0 = reverse (rdUpdated raw)
       in if Map.null rawRem && Map.null rawAdd
            then ([], respawned, updated0)
            else
              if Map.null rawRem
                then ([], respawned <> Map.elems rawAdd, updated0)
                else
                  if Map.null rawAdd
                    then (Map.keys rawRem, respawned, updated0)
                    else
                      let netRemoved = Map.keys (Map.difference rawRem rawAdd)
                          netAdded = Map.elems (Map.difference rawAdd rawRem)
                          reconciled = Map.intersectionWith (,) rawRem rawAdd
                          (recRespawned, recUpdated) =
                            Map.foldl' reconcileStep ([], []) reconciled
                       in ( netRemoved
                          , respawned <> netAdded <> reverse recRespawned
                          , updated0 <> reverse recUpdated
                          )
  where
    reconcileStep (addAcc, updAcc) (oldB, newB) =
      case diffBody oldB newB of
        BodyUnchanged -> (addAcc, updAcc)
        BodyRespawn b -> (b : addAcc, updAcc)
        BodyModified u -> (addAcc, u : updAcc)

-- | Compute the minimal 'ScenePatch' from the previous 'DiffState' to the next 'Scene'.
diffScene
  :: (Ord bid, Ord tid)
  => Maybe (DiffState bid tid)
  -> Scene bid tid
  -> (ScenePatch bid tid, DiffState bid tid)
diffScene Nothing next =
  (PatchReset next, DiffState next)
diffScene (Just prevSt) next
  | sceneEpoch prev /= sceneEpoch next =
      (PatchReset next, DiffState next)
  | sameRef prev next =
      (PatchDelta emptyDelta, prevSt)
  | otherwise =
      let (remIds, addBodies, updBodies) =
            diffTree (sceneTree prev) (sceneTree next)
          delta =
            SceneDelta
              { sdEpoch = sceneEpoch next
              , sdGravity = diffField (sceneGravity prev) (sceneGravity next)
              , sdBackground = diffField (sceneBackground prev) (sceneBackground next)
              , sdFog = diffField (sceneFog prev) (sceneFog next)
              , sdCamera = diffField (sceneCamera prev) (sceneCamera next)
              , sdLights = diffField (sceneLights prev) (sceneLights next)
              , sdTimers = diffField (sceneTimers prev) (sceneTimers next)
              , sdHud = diffField (sceneHud prev) (sceneHud next)
              , sdRemoved = remIds
              , sdAdded = addBodies
              , sdUpdated = updBodies
              }
       in (PatchDelta delta, DiffState next)
  where
    prev = dsScene prevSt
    emptyDelta =
      SceneDelta
        { sdEpoch = sceneEpoch next
        , sdGravity = Nothing
        , sdBackground = Nothing
        , sdFog = Nothing
        , sdCamera = Nothing
        , sdLights = Nothing
        , sdTimers = Nothing
        , sdHud = Nothing
        , sdRemoved = []
        , sdAdded = []
        , sdUpdated = []
        }

    diffField :: Eq a => a -> a -> Maybe a
    diffField a b = if eqFast a b then Nothing else Just b
