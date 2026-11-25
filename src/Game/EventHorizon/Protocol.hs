{-# LANGUAGE OverloadedStrings #-}

-- | Wire protocol, bidirectional ID interning, delta patch encoding,
-- and batched client message decoding.
module Game.EventHorizon.Protocol
  ( -- * Bidirectional ID Interning
    IdTable (..)
  , emptyIdTable
  , internId
  , retireId
  , lookupWireId
  , resetIdTable

    -- * Connection Wire State
  , WireState (..)
  , emptyWireState

    -- * Raw Client Batch Decoding
  , RawEvent (..)
  , RawClientBatch (..)
  , decodeClientBatch
  , resolveClientBatch
  , parseKey

    -- * Patch & Command Encoding
  , encodePatch
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, (.=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toLower)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Game.EventHorizon.Diff
import Game.EventHorizon.Math
import Game.EventHorizon.Scene
import Game.EventHorizon.Types
import Text.Read (readMaybe)

-- | Bidirectional intern table mapping user-defined IDs to compact integer wire IDs.
-- Retains a bounded set of recently retired IDs so in-flight WebSocket messages
-- referencing a just-removed body or timer decode cleanly without memory leaks.
data IdTable a = IdTable
  { idNext :: !Int
  , idToWire :: !(Map a Int)
  , idFromWire :: !(IntMap a)
  , idRetired :: !(IntMap a)
  }
  deriving (Eq, Show)

emptyIdTable :: IdTable a
emptyIdTable = IdTable 1 Map.empty IM.empty IM.empty

internId :: Ord a => a -> IdTable a -> (Int, IdTable a)
internId x tbl =
  case Map.lookup x (idToWire tbl) of
    Just wid -> (wid, tbl)
    Nothing ->
      let wid = idNext tbl
          tbl' =
            tbl
              { idNext = wid + 1
              , idToWire = Map.insert x wid (idToWire tbl)
              , idFromWire = IM.insert wid x (idFromWire tbl)
              }
       in (wid, tbl')

retireId :: Ord a => a -> IdTable a -> (Maybe Int, IdTable a)
retireId x tbl =
  case Map.lookup x (idToWire tbl) of
    Nothing -> (Nothing, tbl)
    Just wid ->
      let retired0 = IM.insert wid x (idRetired tbl)
          retired1 =
            if IM.size retired0 > 512
              then IM.deleteMin retired0
              else retired0
          tbl' =
            tbl
              { idToWire = Map.delete x (idToWire tbl)
              , idFromWire = IM.delete wid (idFromWire tbl)
              , idRetired = retired1
              }
       in (Just wid, tbl')

lookupWireId :: Int -> IdTable a -> Maybe a
lookupWireId wid tbl =
  case IM.lookup wid (idFromWire tbl) of
    Just x -> Just x
    Nothing -> IM.lookup wid (idRetired tbl)

-- | Reset active/retired mappings on epoch change while keeping wire IDs monotonic
-- so stale wire IDs can never accidentally alias new bodies.
resetIdTable :: IdTable a -> IdTable a
resetIdTable tbl =
  tbl
    { idToWire = Map.empty
    , idFromWire = IM.empty
    , idRetired = IM.empty
    }

-- | Per-connection protocol state: intern tables, current epoch, and cached snapshot.
data WireState bid tid = WireState
  { wsEpoch :: !Int
  , wsBodies :: !(IdTable bid)
  , wsTimers :: !(IdTable tid)
  , wsSnapshot :: !(Snapshot bid)
  }
  deriving (Eq, Show)

emptyWireState :: WireState bid tid
emptyWireState = WireState (-1) emptyIdTable emptyIdTable emptySnapshot

-- | Unresolved wire event received from the browser client.
data RawEvent
  = RawReady
  | RawKeyDown !Key
  | RawKeyUp !Key
  | RawMouseDown !MouseButton !(Maybe (Int, Vec3))
  | RawMouseUp !MouseButton
  | RawCollisionStart !Int !Int
  | RawCollisionEnd !Int !Int
  | RawTimerFired !Int
  deriving (Eq, Show)

-- | Batched message sent from the browser client on input, physics step, or timer.
data RawClientBatch = RawClientBatch
  { rcbEpoch :: !Int
  , rcbTime :: !Double
  , rcbEvents :: ![RawEvent]
  , rcbSnapshotDelta :: !(IntMap BodyState)
  }
  deriving (Eq, Show)

decodeClientBatch :: LBS.ByteString -> Either String RawClientBatch
decodeClientBatch = Aeson.eitherDecode

-- | Resolve a client batch against the current 'WireState':
--
-- * Discards batches belonging to a stale epoch (except 'RawReady').
-- * Merges incoming snapshot deltas onto the cached 'Snapshot'.
-- * Translates integer wire IDs back into the user's @bid@ and @tid@ types.
resolveClientBatch
  :: Ord bid
  => RawClientBatch
  -> WireState bid tid
  -> Maybe ([Event bid tid], Snapshot bid, WireState bid tid)
resolveClientBatch batch ws
  | hasReady = Just ([EngineReady], updatedSnap, ws {wsSnapshot = updatedSnap})
  | rcbEpoch batch /= wsEpoch ws = Nothing
  | otherwise =
      let resolvedEvents = catMaybes (map resolveEv (rcbEvents batch))
       in Just (resolvedEvents, updatedSnap, ws {wsSnapshot = updatedSnap})
  where
    hasReady = RawReady `elem` rcbEvents batch

    mergedBodies =
      IM.foldlWithKey'
        ( \acc wid bstate ->
            case lookupWireId wid (wsBodies ws) of
              Just bid -> Map.insert bid bstate acc
              Nothing -> acc
        )
        (snapBodies (wsSnapshot ws))
        (rcbSnapshotDelta batch)

    updatedSnap =
      Snapshot
        { snapTime = rcbTime batch
        , snapBodies = mergedBodies
        }

    resolveEv raw = case raw of
      RawReady -> Just EngineReady
      RawKeyDown k -> Just (KeyDown k)
      RawKeyUp k -> Just (KeyUp k)
      RawMouseDown btn mHit ->
        let resolvedHit = do
              (wid, pt) <- mHit
              bid <- lookupWireId wid (wsBodies ws)
              pure (bid, pt)
         in Just (MouseDown btn resolvedHit)
      RawMouseUp btn -> Just (MouseUp btn)
      RawCollisionStart wa wb -> do
        a <- lookupWireId wa (wsBodies ws)
        b <- lookupWireId wb (wsBodies ws)
        pure (CollisionStart a b)
      RawCollisionEnd wa wb -> do
        a <- lookupWireId wa (wsBodies ws)
        b <- lookupWireId wb (wsBodies ws)
        pure (CollisionEnd a b)
      RawTimerFired wt -> do
        t <- lookupWireId wt (wsTimers ws)
        pure (TimerFired t)

-- | Encode a 'ScenePatch' and one-shot commands into compact JSON while updating
-- the connection's intern tables and snapshot cache.
encodePatch
  :: (Ord bid, Ord tid)
  => ScenePatch bid tid
  -> [Command bid]
  -> WireState bid tid
  -> (LBS.ByteString, WireState bid tid)
encodePatch patch cmds ws0 =
  case patch of
    PatchReset sc ->
      let bTbl0 = resetIdTable (wsBodies ws0)
          tTbl0 = resetIdTable (wsTimers ws0)
          sceneBodyList = allSceneBodies sc
          (bodyVals, bTbl1) = mapAccumL' encodeBody bTbl0 sceneBodyList
          (camVal, bTbl2) = encodeCamera bTbl1 (sceneCamera sc)
          (timerVals, tTbl1) = mapAccumL' encodeTimer tTbl0 (sceneTimers sc)
          (cmdVals, bTbl3) = mapAccumL' encodeCommand bTbl2 cmds
          seededSnap =
            Snapshot
              { snapTime = snapTime (wsSnapshot ws0)
              , snapBodies = Map.fromList [(bodyId b, initialBodyState b) | b <- sceneBodyList]
              }
          ws1 =
            WireState
              { wsEpoch = sceneEpoch sc
              , wsBodies = bTbl3
              , wsTimers = tTbl1
              , wsSnapshot = seededSnap
              }
          val =
            Aeson.object
              [ "type" .= ("reset" :: Text)
              , "epoch" .= sceneEpoch sc
              , "gravity" .= vec3ToJSON (sceneGravity sc)
              , "background" .= colorToJSON (sceneBackground sc)
              , "fog" .= fmap fogToJSON (sceneFog sc)
              , "camera" .= camVal
              , "lights" .= map lightToJSON (sceneLights sc)
              , "bodies" .= bodyVals
              , "timers" .= timerVals
              , "hud" .= map hudToJSON (sceneHud sc)
              , "commands" .= cmdVals
              ]
       in (Aeson.encode val, ws1)
    PatchDelta d ->
      let (remWids, bTbl1) = retireMany (sdRemoved d) (wsBodies ws0)
          (addVals, bTbl2) = mapAccumL' encodeBody bTbl1 (sdAdded d)
          (updVals, bTbl3) = mapAccumL' encodeBodyUpdate bTbl2 (sdUpdated d)
          (mCamVal, bTbl4) = case sdCamera d of
            Nothing -> (Nothing, bTbl3)
            Just cam -> let (v, t) = encodeCamera bTbl3 cam in (Just v, t)
          (mTimerVals, tTbl1) = case sdTimers d of
            Nothing -> (Nothing, wsTimers ws0)
            Just ts ->
              let activeTids = map timerKey ts
                  staleTids = filter (`notElem` activeTids) (Map.keys (idToWire (wsTimers ws0)))
                  (_, tTblClean) = retireMany staleTids (wsTimers ws0)
                  (vs, t) = mapAccumL' encodeTimer tTblClean ts
               in (Just vs, t)
          (cmdVals, bTbl5) = mapAccumL' encodeCommand bTbl4 cmds

          -- Keep server snapshot cache synchronized with removed/added/moved/teleported bodies.
          snapBodies1 = foldl' (flip Map.delete) (snapBodies (wsSnapshot ws0)) (sdRemoved d)
          snapBodies2 =
            foldl'
              (\m b -> Map.insert (bodyId b) (initialBodyState b) m)
              snapBodies1
              (sdAdded d)
          snapBodies3 =
            foldl'
              ( \m u -> case buPose u of
                  Nothing -> m
                  Just (Pose p r) ->
                    Map.adjust (\bs -> bs {bsPos = p, bsRot = r}) (buId u) m
              )
              snapBodies2
              (sdUpdated d)
          snapBodies4 = foldl' applyCommandToSnap snapBodies3 cmds

          ws1 =
            ws0
              { wsEpoch = sdEpoch d
              , wsBodies = bTbl5
              , wsTimers = tTbl1
              , wsSnapshot = (wsSnapshot ws0) {snapBodies = snapBodies4}
              }

          fields =
            catMaybes
              [ Just ("type" .= ("delta" :: Text))
              , Just ("epoch" .= sdEpoch d)
              , fmap (\g -> "gravity" .= vec3ToJSON g) (sdGravity d)
              , fmap (\bg -> "background" .= colorToJSON bg) (sdBackground d)
              , fmap (\fg -> "fog" .= fmap fogToJSON fg) (sdFog d)
              , fmap (\c -> "camera" .= c) mCamVal
              , fmap (\ls -> "lights" .= map lightToJSON ls) (sdLights d)
              , fmap (\ts -> "timers" .= ts) mTimerVals
              , fmap (\hs -> "hud" .= map hudToJSON hs) (sdHud d)
              , if null remWids then Nothing else Just ("removed" .= remWids)
              , if null addVals then Nothing else Just ("added" .= addVals)
              , if null updVals then Nothing else Just ("updated" .= updVals)
              , if null cmdVals then Nothing else Just ("commands" .= cmdVals)
              ]
       in (Aeson.encode (Aeson.object fields), ws1)

initialBodyState :: Body bid -> BodyState
initialBodyState b =
  BodyState
    { bsPos = posePos (bodyPose b)
    , bsRot = poseRot (bodyPose b)
    , bsVel = bodyInitialVel b
    , bsAngVel = vZero
    }

applyCommandToSnap :: Ord bid => Map bid BodyState -> Command bid -> Map bid BodyState
applyCommandToSnap m cmd = case cmd of
  Teleport bid (Pose p r) -> Map.adjust (\bs -> bs {bsPos = p, bsRot = r}) bid m
  SetVelocity bid v -> Map.adjust (\bs -> bs {bsVel = v}) bid m
  SetAngularVelocity bid av -> Map.adjust (\bs -> bs {bsAngVel = av}) bid m
  _ -> m

retireMany :: Ord a => [a] -> IdTable a -> ([Int], IdTable a)
retireMany xs tbl0 =
  let (revIds, tbl1) =
        foldl'
          ( \(acc, t) x ->
              case retireId x t of
                (Just wid, t') -> (wid : acc, t')
                (Nothing, t') -> (acc, t')
          )
          ([], tbl0)
          xs
   in (reverse revIds, tbl1)

mapAccumL' :: (s -> a -> (b, s)) -> s -> [a] -> ([b], s)
mapAccumL' f s0 = go s0 []
  where
    go s acc [] = (reverse acc, s)
    go s acc (x : xs) =
      let (!y, !s') = f s x
       in go s' (y : acc) xs

-- | Parse browser KeyboardEvent key/code strings into 'Key'.
parseKey :: Text -> Key
parseKey raw =
  case raw of
    "ArrowUp" -> KeyUpArrow
    "ArrowDown" -> KeyDownArrow
    "ArrowLeft" -> KeyLeftArrow
    "ArrowRight" -> KeyRightArrow
    "Space" -> KeySpace
    " " -> KeySpace
    "Enter" -> KeyEnter
    "Escape" -> KeyEscape
    "ShiftLeft" -> KeyShift
    "ShiftRight" -> KeyShift
    "Shift" -> KeyShift
    "Tab" -> KeyTab
    "Backspace" -> KeyBackspace
    _ ->
      case T.stripPrefix "Key" raw of
        Just rest | T.length rest == 1 -> KeyChar (toLower (T.head rest))
        _ ->
          case T.stripPrefix "Digit" raw of
            Just rest | T.length rest == 1 -> KeyChar (T.head rest)
            _ ->
              if T.length raw == 1
                then KeyChar (toLower (T.head raw))
                else KeyOther raw

vec3ToJSON :: Vec3 -> Value
vec3ToJSON (Vec3 x y z) = toJSON [x, y, z]

quatToJSON :: Quat -> Value
quatToJSON (Quat x y z w) = toJSON [x, y, z, w]

colorToJSON :: Color -> Value
colorToJSON (Color r g b) = toJSON [r, g, b]

poseToJSON :: Pose -> Value
poseToJSON (Pose p r) =
  Aeson.object
    [ "pos" .= vec3ToJSON p
    , "rot" .= quatToJSON r
    ]

fogToJSON :: (Color, Double, Double) -> Value
fogToJSON (c, near, far) =
  Aeson.object
    [ "color" .= colorToJSON c
    , "near" .= near
    , "far" .= far
    ]

materialToJSON :: Material -> Value
materialToJSON m =
  Aeson.object
    [ "color" .= fmap colorToJSON (matColor m)
    , "emissive"
        .= fmap
          (\(c, i) -> Aeson.object ["color" .= colorToJSON c, "intensity" .= i])
          (matEmissive m)
    , "roughness" .= matRoughness m
    , "metalness" .= matMetalness m
    , "opacity" .= matOpacity m
    , "wireframe" .= matWireframe m
    , "castShadow" .= matCastShadow m
    , "receiveShadow" .= matReceiveShadow m
    ]

meshToJSON :: Mesh -> Value
meshToJSON m = case m of
  EmptyMesh -> Aeson.object ["type" .= ("empty" :: Text)]
  BoxMesh sz ->
    Aeson.object ["type" .= ("box" :: Text), "size" .= vec3ToJSON sz]
  SphereMesh r ->
    Aeson.object ["type" .= ("sphere" :: Text), "radius" .= r]
  CylinderMesh rt rb h ->
    Aeson.object
      [ "type" .= ("cylinder" :: Text)
      , "radiusTop" .= rt
      , "radiusBottom" .= rb
      , "height" .= h
      ]
  ConeMesh r h ->
    Aeson.object ["type" .= ("cone" :: Text), "radius" .= r, "height" .= h]
  CapsuleMesh r h ->
    Aeson.object ["type" .= ("capsule" :: Text), "radius" .= r, "height" .= h]
  TorusMesh r tube ->
    Aeson.object ["type" .= ("torus" :: Text), "radius" .= r, "tube" .= tube]
  PlaneMesh w h ->
    Aeson.object ["type" .= ("plane" :: Text), "width" .= w, "height" .= h]
  ColorMesh c inner ->
    Aeson.object
      [ "type" .= ("color" :: Text)
      , "color" .= colorToJSON c
      , "child" .= meshToJSON inner
      ]
  EmissiveMesh c intensity inner ->
    Aeson.object
      [ "type" .= ("emissive" :: Text)
      , "color" .= colorToJSON c
      , "intensity" .= intensity
      , "child" .= meshToJSON inner
      ]
  MaterialMesh mat inner ->
    Aeson.object
      [ "type" .= ("material" :: Text)
      , "material" .= materialToJSON mat
      , "child" .= meshToJSON inner
      ]
  TranslateMesh v inner ->
    Aeson.object
      [ "type" .= ("translate" :: Text)
      , "offset" .= vec3ToJSON v
      , "child" .= meshToJSON inner
      ]
  RotateMesh v inner ->
    Aeson.object
      [ "type" .= ("rotate" :: Text)
      , "euler" .= vec3ToJSON v
      , "child" .= meshToJSON inner
      ]
  ScaleMesh v inner ->
    Aeson.object
      [ "type" .= ("scale" :: Text)
      , "factor" .= vec3ToJSON v
      , "child" .= meshToJSON inner
      ]
  GroupMesh ms ->
    Aeson.object
      [ "type" .= ("group" :: Text)
      , "children" .= map meshToJSON ms
      ]

colliderToJSON :: Collider -> Value
colliderToJSON col = case col of
  NoCollider -> Aeson.object ["type" .= ("none" :: Text)]
  BoxCollider sz ->
    Aeson.object ["type" .= ("box" :: Text), "size" .= vec3ToJSON sz]
  SphereCollider r ->
    Aeson.object ["type" .= ("sphere" :: Text), "radius" .= r]
  CylinderCollider r h ->
    Aeson.object ["type" .= ("cylinder" :: Text), "radius" .= r, "height" .= h]
  CapsuleCollider r h ->
    Aeson.object ["type" .= ("capsule" :: Text), "radius" .= r, "height" .= h]
  CompoundCollider parts ->
    Aeson.object
      [ "type" .= ("compound" :: Text)
      , "parts"
          .= [ Aeson.object ["pose" .= poseToJSON p, "collider" .= colliderToJSON c]
             | (p, c) <- parts
             ]
      ]

bodyKindToJSON :: BodyKind -> Value
bodyKindToJSON k = case k of
  Static -> Aeson.object ["type" .= ("static" :: Text)]
  Dynamic -> Aeson.object ["type" .= ("dynamic" :: Text)]
  Kinematic spd ->
    Aeson.object ["type" .= ("kinematic" :: Text), "speed" .= spd]
  Sensor -> Aeson.object ["type" .= ("sensor" :: Text)]

driveToJSON :: Drive -> Value
driveToJSON d = case d of
  NoDrive -> Aeson.object ["type" .= ("none" :: Text)]
  WalkDrive v ->
    Aeson.object ["type" .= ("walk" :: Text), "vel" .= vec3ToJSON v]
  VelocityDrive lv av ->
    Aeson.object
      [ "type" .= ("velocity" :: Text)
      , "linVel" .= vec3ToJSON lv
      , "angVel" .= vec3ToJSON av
      ]
  ForceDrive f tq ->
    Aeson.object
      [ "type" .= ("force" :: Text)
      , "force" .= vec3ToJSON f
      , "torque" .= vec3ToJSON tq
      ]
  SeekDrive tgt spd maxF ->
    Aeson.object
      [ "type" .= ("seek" :: Text)
      , "target" .= vec3ToJSON tgt
      , "speed" .= spd
      , "maxForce" .= maxF
      ]
  FlightDrive spd p y r al ->
    Aeson.object
      [ "type" .= ("flight" :: Text)
      , "speed" .= spd
      , "pitch" .= p
      , "yaw" .= y
      , "roll" .= r
      , "autoLevel" .= al
      ]

encodeBody :: Ord bid => IdTable bid -> Body bid -> (Value, IdTable bid)
encodeBody tbl b =
  let (wid, tbl') = internId (bodyId b) tbl
      CollisionFilter grp msk = bodyFilter b
      val =
        Aeson.object
          [ "id" .= wid
          , "kind" .= bodyKindToJSON (bodyKind b)
          , "pose" .= poseToJSON (bodyPose b)
          , "collider" .= colliderToJSON (bodyCollider b)
          , "mesh" .= meshToJSON (bodyMesh b)
          , "initialVel" .= vec3ToJSON (bodyInitialVel b)
          , "mass" .= bodyMass b
          , "friction" .= bodyFriction b
          , "restitution" .= bodyRestitution b
          , "linearDamping" .= fst (bodyDamping b)
          , "angularDamping" .= snd (bodyDamping b)
          , "lockRot" .= bodyLockRot b
          , "gravityScale" .= bodyGravity b
          , "ccd" .= bodyCCD b
          , "drive" .= driveToJSON (bodyDrive b)
          , "filterGroup" .= grp
          , "filterMask" .= msk
          , "events" .= bodyEvents b
          ]
   in (val, tbl')

encodeBodyUpdate :: Ord bid => IdTable bid -> BodyUpdate bid -> (Value, IdTable bid)
encodeBodyUpdate tbl u =
  let (wid, tbl') = internId (buId u) tbl
      fields =
        catMaybes
          [ Just ("id" .= wid)
          , fmap (\m -> "mesh" .= meshToJSON m) (buMesh u)
          , fmap (\p -> "pose" .= poseToJSON p) (buPose u)
          , fmap (\d -> "drive" .= driveToJSON d) (buDrive u)
          , fmap (\s -> "kinematicSpeed" .= s) (buKinematicSpeed u)
          , fmap (\g -> "gravityScale" .= g) (buGravity u)
          , fmap (\(ld, _) -> "linearDamping" .= ld) (buDamping u)
          , fmap (\(_, ad) -> "angularDamping" .= ad) (buDamping u)
          ]
   in (Aeson.object fields, tbl')

encodeCamera :: Ord bid => IdTable bid -> Camera bid -> (Value, IdTable bid)
encodeCamera tbl cam = case cam of
  FixedCamera p l fov ->
    ( Aeson.object
        [ "type" .= ("fixed" :: Text)
        , "pos" .= vec3ToJSON p
        , "lookAt" .= vec3ToJSON l
        , "fov" .= fov
        ]
    , tbl
    )
  FollowCamera tgt off loff loc sm fov ->
    let (wid, tbl') = internId tgt tbl
     in ( Aeson.object
            [ "type" .= ("follow" :: Text)
            , "target" .= wid
            , "offset" .= vec3ToJSON off
            , "lookOffset" .= vec3ToJSON loff
            , "local" .= loc
            , "smooth" .= sm
            , "fov" .= fov
            ]
        , tbl'
        )

lightToJSON :: Light -> Value
lightToJSON l = case l of
  AmbientLight c i ->
    Aeson.object
      [ "type" .= ("ambient" :: Text)
      , "color" .= colorToJSON c
      , "intensity" .= i
      ]
  HemisphereLight sky gnd i ->
    Aeson.object
      [ "type" .= ("hemisphere" :: Text)
      , "skyColor" .= colorToJSON sky
      , "groundColor" .= colorToJSON gnd
      , "intensity" .= i
      ]
  DirectionalLight c i p ->
    Aeson.object
      [ "type" .= ("directional" :: Text)
      , "color" .= colorToJSON c
      , "intensity" .= i
      , "pos" .= vec3ToJSON p
      ]
  PointLight c i p dist ->
    Aeson.object
      [ "type" .= ("point" :: Text)
      , "color" .= colorToJSON c
      , "intensity" .= i
      , "pos" .= vec3ToJSON p
      , "distance" .= dist
      ]

timerKey :: Timer tid -> tid
timerKey (Once tid _) = tid
timerKey (Every tid _) = tid

encodeTimer :: Ord tid => IdTable tid -> Timer tid -> (Value, IdTable tid)
encodeTimer tbl t = case t of
  Once tid sec ->
    let (wid, tbl') = internId tid tbl
     in ( Aeson.object
            [ "id" .= wid
            , "mode" .= ("once" :: Text)
            , "seconds" .= sec
            ]
        , tbl'
        )
  Every tid sec ->
    let (wid, tbl') = internId tid tbl
     in ( Aeson.object
            [ "id" .= wid
            , "mode" .= ("every" :: Text)
            , "seconds" .= sec
            ]
        , tbl'
        )

waveToText :: WaveType -> Text
waveToText w = case w of
  WaveSine -> "sine"
  WaveSquare -> "square"
  WaveSawtooth -> "sawtooth"
  WaveTriangle -> "triangle"

soundToJSON :: Sound -> Value
soundToJSON s = case s of
  SoundTone w f0 f1 dur vol ->
    Aeson.object
      [ "type" .= ("tone" :: Text)
      , "wave" .= waveToText w
      , "freqStart" .= f0
      , "freqEnd" .= f1
      , "duration" .= dur
      , "volume" .= vol
      ]
  SoundNoise dur vol ->
    Aeson.object
      [ "type" .= ("noise" :: Text)
      , "duration" .= dur
      , "volume" .= vol
      ]

encodeCommand :: Ord bid => IdTable bid -> Command bid -> (Value, IdTable bid)
encodeCommand tbl cmd = case cmd of
  ApplyImpulse bid v ->
    let (wid, tbl') = internId bid tbl
     in (Aeson.object ["type" .= ("impulse" :: Text), "id" .= wid, "vec" .= vec3ToJSON v], tbl')
  ApplyTorqueImpulse bid v ->
    let (wid, tbl') = internId bid tbl
     in (Aeson.object ["type" .= ("torqueImpulse" :: Text), "id" .= wid, "vec" .= vec3ToJSON v], tbl')
  SetVelocity bid v ->
    let (wid, tbl') = internId bid tbl
     in (Aeson.object ["type" .= ("setVel" :: Text), "id" .= wid, "vec" .= vec3ToJSON v], tbl')
  SetAngularVelocity bid v ->
    let (wid, tbl') = internId bid tbl
     in (Aeson.object ["type" .= ("setAngVel" :: Text), "id" .= wid, "vec" .= vec3ToJSON v], tbl')
  Teleport bid pose ->
    let (wid, tbl') = internId bid tbl
     in (Aeson.object ["type" .= ("teleport" :: Text), "id" .= wid, "pose" .= poseToJSON pose], tbl')
  PlaySound sndFx ->
    (Aeson.object ["type" .= ("sound" :: Text), "sound" .= soundToJSON sndFx], tbl)

anchorToText :: HudAnchor -> Text
anchorToText a = case a of
  TopLeft -> "top-left"
  TopCenter -> "top-center"
  TopRight -> "top-right"
  Center -> "center"
  BottomCenter -> "bottom-center"

hudToJSON :: HudElement -> Value
hudToJSON h = case h of
  HudText anchor txt ->
    Aeson.object
      [ "type" .= ("text" :: Text)
      , "anchor" .= anchorToText anchor
      , "text" .= txt
      ]
  HudCrosshair ->
    Aeson.object ["type" .= ("crosshair" :: Text)]
  HudBanner title sub ->
    Aeson.object
      [ "type" .= ("banner" :: Text)
      , "title" .= title
      , "subtitle" .= sub
      ]

parseVec3 :: Value -> Aeson.Parser Vec3
parseVec3 v = do
  xs <- parseJSON v
  case xs of
    [x, y, z] -> pure (Vec3 x y z)
    _ -> fail "Expected 3-element Vec3 array"

parseQuat :: Value -> Aeson.Parser Quat
parseQuat v = do
  xs <- parseJSON v
  case xs of
    [x, y, z, w] -> pure (Quat x y z w)
    _ -> fail "Expected 4-element Quat array"

parseMouseButton :: Int -> Aeson.Parser MouseButton
parseMouseButton n = case n of
  0 -> pure MouseLeft
  1 -> pure MouseMiddle
  2 -> pure MouseRight
  _ -> pure MouseLeft

parseBodyState :: Value -> Aeson.Parser BodyState
parseBodyState = Aeson.withObject "BodyState" $ \o -> do
  p <- o .: "pos" >>= parseVec3
  r <- o .: "rot" >>= parseQuat
  lv <- o .: "vel" >>= parseVec3
  av <- o .: "angVel" >>= parseVec3
  pure (BodyState p r lv av)

parseSnapshotDelta :: Maybe Aeson.Object -> Aeson.Parser (IntMap BodyState)
parseSnapshotDelta Nothing = pure IM.empty
parseSnapshotDelta (Just obj) =
  foldl'
    ( \accM (k, v) -> do
        acc <- accM
        case readMaybe (T.unpack (Key.toText k)) of
          Nothing -> pure acc
          Just wid -> do
            bs <- parseBodyState v
            pure (IM.insert wid bs acc)
    )
    (pure IM.empty)
    (KM.toList obj)

instance FromJSON RawEvent where
  parseJSON = Aeson.withObject "RawEvent" $ \o -> do
    evType <- o .: "type" :: Aeson.Parser Text
    case evType of
      "ready" -> pure RawReady
      "keydown" -> RawKeyDown . parseKey <$> o .: "key"
      "keyup" -> RawKeyUp . parseKey <$> o .: "key"
      "mousedown" -> do
        btn <- o .: "button" >>= parseMouseButton
        mHitObj <- o .:? "hit"
        mHit <- case mHitObj of
          Nothing -> pure Nothing
          Just hv ->
            Aeson.withObject
              "Hit"
              (\ho -> (,) <$> ho .: "id" <*> (ho .: "point" >>= parseVec3))
              hv
              >>= pure . Just
        pure (RawMouseDown btn mHit)
      "mouseup" -> RawMouseUp <$> (o .: "button" >>= parseMouseButton)
      "collisionStart" -> RawCollisionStart <$> o .: "a" <*> o .: "b"
      "collisionEnd" -> RawCollisionEnd <$> o .: "a" <*> o .: "b"
      "timer" -> RawTimerFired <$> o .: "id"
      other -> fail ("Unknown event type: " <> T.unpack other)

instance FromJSON RawClientBatch where
  parseJSON = Aeson.withObject "RawClientBatch" $ \o -> do
    ep <- o .: "epoch"
    tm <- o .: "time"
    evs <- o .: "events"
    rawDelta <- o .:? "snapshotDelta"
    snapDelta <- parseSnapshotDelta rawDelta
    pure (RawClientBatch ep tm evs snapDelta)
