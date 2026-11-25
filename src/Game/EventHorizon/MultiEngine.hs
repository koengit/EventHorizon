{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Multi-user runtime engines (local multi-tab server, networked authoritative
-- server, and client-side predicting rollback engine) backed by a deterministic
-- headless Node Rapier3D physics sidecar.
module Game.EventHorizon.MultiEngine
  ( -- * Network Wire Protocol
    RawInput (..)
  , NetMsg (..)

    -- * Headless Rapier3D Sidecar Driver
  , HeadlessPhysics
  , StepResult (..)
  , startHeadlessPhysics
  , stopHeadlessPhysics
  , hpSendPatch
  , hpStep
  , hpTakeSnapshot
  , hpRestoreSnapshot

    -- * Multi-User Entry Points
  , playLocalMulti
  , serveMulti
  , connectMulti
  ) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, withMVar)
import Control.Exception (SomeException, bracket, catch, finally)
import Control.Monad (foldM, forM_, unless, void, when)
import Data.Aeson (FromJSON (..), ToJSON (..), Value, (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', sortBy, stripPrefix)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Game.EventHorizon.Diff
import Game.EventHorizon.Engine (Config (..))
import Game.EventHorizon.Math
import Game.EventHorizon.Memo (advanceMemoGen)
import Game.EventHorizon.Multi
import qualified Game.EventHorizon.Protocol as P
import Game.EventHorizon.Rollback
import Game.EventHorizon.Scene (Scene)
import Game.EventHorizon.Types
import Network.HTTP.Types (hContentType, status200, status404)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS
import Paths_event_horizon (getDataFileName)
import System.Directory (doesFileExist)
import System.IO (Handle, hFlush, stdout)
import System.Process
  ( CreateProcess (..)
  , ProcessHandle
  , StdStream (CreatePipe)
  , createProcess
  , proc
  , terminateProcess
  , waitForProcess
  )
import Text.Read (readMaybe)

--------------------------------------------------------------------------------
-- Wire Protocol (Input-Only Best-Case Traffic)
--------------------------------------------------------------------------------

-- | Unresolved player input transmitted over the multiplayer network.
data RawInput
  = RawInKeyDown !Key
  | RawInKeyUp !Key
  | RawInMouseDown !MouseButton !(Maybe (Int, Vec3))
  | RawInMouseUp !MouseButton
  deriving (Eq, Show)

-- | Compact multiplayer network message exchanged between 'serveMulti' and
-- predicting 'connectMulti' clients. Normal operation only transmits inputs
-- and periodic lightweight snapshot hashes — never full game state.
data NetMsg
  = NetJoin
  | NetWelcome !Int !Tick
  | NetPlayerJoined !Int !Tick
  | NetPlayerLeft !Int !Tick
  | NetClientInput !Tick !RawInput
  | NetTickInputs !Tick ![(Int, RawInput)]
  | NetConfirm !Tick !Word64
  | NetPhysicsSync !Tick !Text
  deriving (Eq, Show)

keyToText :: Key -> Text
keyToText k = case k of
  KeyChar c -> "char:" <> T.singleton c
  KeyUpArrow -> "ArrowUp"
  KeyDownArrow -> "ArrowDown"
  KeyLeftArrow -> "ArrowLeft"
  KeyRightArrow -> "ArrowRight"
  KeySpace -> "Space"
  KeyEnter -> "Enter"
  KeyEscape -> "Escape"
  KeyShift -> "Shift"
  KeyTab -> "Tab"
  KeyBackspace -> "Backspace"
  KeyOther t -> "other:" <> t

textToKey :: Text -> Key
textToKey raw = case T.stripPrefix "char:" raw of
  Just rest | T.length rest == 1 -> KeyChar (T.head rest)
  _ -> case T.stripPrefix "other:" raw of
    Just rest -> KeyOther rest
    Nothing -> P.parseKey raw

mouseBtnToInt :: MouseButton -> Int
mouseBtnToInt MouseLeft = 0
mouseBtnToInt MouseMiddle = 1
mouseBtnToInt MouseRight = 2

intToMouseBtn :: Int -> MouseButton
intToMouseBtn 0 = MouseLeft
intToMouseBtn 1 = MouseMiddle
intToMouseBtn 2 = MouseRight
intToMouseBtn _ = MouseLeft

vec3ToVal :: Vec3 -> Value
vec3ToVal (Vec3 x y z) = toJSON [x, y, z]

quatToVal :: Quat -> Value
quatToVal (Quat x y z w) = toJSON [x, y, z, w]

parseVec3Val :: Value -> Aeson.Parser Vec3
parseVec3Val v = do
  xs <- parseJSON v
  case xs of
    [x, y, z] -> pure (Vec3 x y z)
    _ -> fail "Expected 3-element Vec3"

parseQuatVal :: Value -> Aeson.Parser Quat
parseQuatVal v = do
  xs <- parseJSON v
  case xs of
    [x, y, z, w] -> pure (Quat x y z w)
    _ -> fail "Expected 4-element Quat"

instance ToJSON RawInput where
  toJSON ri = case ri of
    RawInKeyDown k ->
      Aeson.object ["t" .= ("kd" :: Text), "k" .= keyToText k]
    RawInKeyUp k ->
      Aeson.object ["t" .= ("ku" :: Text), "k" .= keyToText k]
    RawInMouseDown btn mHit ->
      Aeson.object
        [ "t" .= ("md" :: Text)
        , "b" .= mouseBtnToInt btn
        , "h" .= fmap (\(wid, pt) -> Aeson.object ["id" .= wid, "p" .= vec3ToVal pt]) mHit
        ]
    RawInMouseUp btn ->
      Aeson.object ["t" .= ("mu" :: Text), "b" .= mouseBtnToInt btn]

instance FromJSON RawInput where
  parseJSON = Aeson.withObject "RawInput" $ \o -> do
    tag <- o .: "t" :: Aeson.Parser Text
    case tag of
      "kd" -> RawInKeyDown . textToKey <$> o .: "k"
      "ku" -> RawInKeyUp . textToKey <$> o .: "k"
      "md" -> do
        b <- intToMouseBtn <$> o .: "b"
        mH <- o .:? "h"
        hit <- case mH of
          Nothing -> pure Nothing
          Just hv ->
            Aeson.withObject
              "Hit"
              (\ho -> (,) <$> ho .: "id" <*> (ho .: "p" >>= parseVec3Val))
              hv
              >>= pure . Just
        pure (RawInMouseDown b hit)
      "mu" -> RawInMouseUp . intToMouseBtn <$> o .: "b"
      other -> fail ("Unknown RawInput tag: " <> T.unpack other)

instance ToJSON NetMsg where
  toJSON msg = case msg of
    NetJoin ->
      Aeson.object ["type" .= ("join" :: Text)]
    NetWelcome p t ->
      Aeson.object ["type" .= ("welcome" :: Text), "pid" .= p, "tick" .= t]
    NetPlayerJoined p t ->
      Aeson.object ["type" .= ("joined" :: Text), "pid" .= p, "tick" .= t]
    NetPlayerLeft p t ->
      Aeson.object ["type" .= ("left" :: Text), "pid" .= p, "tick" .= t]
    NetClientInput t inp ->
      Aeson.object ["type" .= ("input" :: Text), "tick" .= t, "input" .= inp]
    NetTickInputs t inps ->
      Aeson.object ["type" .= ("tickInputs" :: Text), "tick" .= t, "inputs" .= inps]
    NetConfirm t h ->
      Aeson.object ["type" .= ("confirm" :: Text), "tick" .= t, "hash" .= h]
    NetPhysicsSync t d ->
      Aeson.object ["type" .= ("sync" :: Text), "tick" .= t, "data" .= d]

instance FromJSON NetMsg where
  parseJSON = Aeson.withObject "NetMsg" $ \o -> do
    ty <- o .: "type" :: Aeson.Parser Text
    case ty of
      "join" -> pure NetJoin
      "welcome" -> NetWelcome <$> o .: "pid" <*> o .: "tick"
      "joined" -> NetPlayerJoined <$> o .: "pid" <*> o .: "tick"
      "left" -> NetPlayerLeft <$> o .: "pid" <*> o .: "tick"
      "input" -> NetClientInput <$> o .: "tick" <*> o .: "input"
      "tickInputs" -> NetTickInputs <$> o .: "tick" <*> o .: "inputs"
      "confirm" -> NetConfirm <$> o .: "tick" <*> o .: "hash"
      "sync" -> NetPhysicsSync <$> o .: "tick" <*> o .: "data"
      other -> fail ("Unknown NetMsg type: " <> T.unpack other)

--------------------------------------------------------------------------------
-- Headless Node Rapier3D Physics Sidecar
--------------------------------------------------------------------------------

-- | Handle to the headless Node Rapier3D physics sidecar process.
data HeadlessPhysics = HeadlessPhysics
  { hpIn :: !Handle
  , hpOut :: !Handle
  , hpProc :: !ProcessHandle
  , hpLock :: !(MVar ())
  }

-- | Result of stepping the headless Rapier3D simulation by one fixed tick.
data StepResult = StepResult
  { srTime :: !Double
  , srCollisions :: ![P.RawEvent]
  , srSnapshotDelta :: !(IntMap BodyState)
  }
  deriving (Eq, Show)

parseBodyStateVal :: Value -> Aeson.Parser BodyState
parseBodyStateVal = Aeson.withObject "BodyState" $ \o -> do
  p <- o .: "pos" >>= parseVec3Val
  r <- o .: "rot" >>= parseQuatVal
  lv <- o .: "vel" >>= parseVec3Val
  av <- o .: "angVel" >>= parseVec3Val
  pure (BodyState p r lv av)

parseSnapDeltaObj :: Maybe Aeson.Object -> Aeson.Parser (IntMap BodyState)
parseSnapDeltaObj Nothing = pure IM.empty
parseSnapDeltaObj (Just obj) =
  foldl'
    ( \accM (k, v) -> do
        acc <- accM
        case readMaybe (T.unpack (Key.toText k)) of
          Nothing -> pure acc
          Just wid -> do
            bs <- parseBodyStateVal v
            pure (IM.insert wid bs acc)
    )
    (pure IM.empty)
    (KM.toList obj)

instance FromJSON StepResult where
  parseJSON = Aeson.withObject "StepResult" $ \o -> do
    tm <- o .: "time"
    cols <- o .: "collisions"
    rawDelta <- o .:? "snapshotDelta"
    snapDelta <- parseSnapDeltaObj rawDelta
    pure (StepResult tm cols snapDelta)

findHeadlessScript :: IO FilePath
findHeadlessScript = do
  let candidates =
        [ "frontend/src/headless-physics.mjs"
        , "./frontend/src/headless-physics.mjs"
        ]
  go candidates
  where
    go [] =
      fail
        "EventHorizon: could not locate frontend/src/headless-physics.mjs (run from project root)"
    go (p : ps) = do
      ex <- doesFileExist p
      if ex then pure p else go ps

-- | Spawn the headless Node Rapier3D sidecar and wait for its ready handshake.
startHeadlessPhysics :: IO HeadlessPhysics
startHeadlessPhysics = do
  scriptPath <- findHeadlessScript
  (mIn, mOut, _, ph) <-
    createProcess
      (proc "node" [scriptPath])
        { std_in = CreatePipe
        , std_out = CreatePipe
        }
  case (mIn, mOut) of
    (Just hIn, Just hOut) -> do
      readyLine <- BS8.hGetLine hOut
      case Aeson.eitherDecodeStrict readyLine of
        Right (Aeson.Object obj)
          | KM.lookup "type" obj == Just (Aeson.String "ready") -> do
              lock <- newMVar ()
              pure (HeadlessPhysics hIn hOut ph lock)
        other -> do
          terminateProcess ph
          void (waitForProcess ph)
          fail ("EventHorizon: unexpected headless physics handshake: " <> show other)
    _ -> fail "EventHorizon: failed to create pipes for headless physics sidecar"

-- | Gracefully shut down the headless Rapier3D sidecar process.
stopHeadlessPhysics :: HeadlessPhysics -> IO ()
stopHeadlessPhysics hp = do
  let quitMsg = Aeson.encode (Aeson.object ["type" .= ("quit" :: Text)])
  (BS.hPutStr (hpIn hp) (LBS.toStrict quitMsg <> "\n") >> hFlush (hpIn hp))
    `catch` (\(_ :: SomeException) -> pure ())
  terminateProcess (hpProc hp) `catch` (\(_ :: SomeException) -> pure ())
  void (waitForProcess (hpProc hp)) `catch` (\(_ :: SomeException) -> pure ())

hpCall :: HeadlessPhysics -> LBS.ByteString -> IO BS.ByteString
hpCall hp payload =
  withMVar (hpLock hp) $ \_ -> do
    BS.hPutStr (hpIn hp) (LBS.toStrict payload <> "\n")
    hFlush (hpIn hp)
    BS8.hGetLine (hpOut hp)

-- | Send an encoded 'ScenePatch' JSON payload (reset or delta) to the headless physics engine.
hpSendPatch :: HeadlessPhysics -> LBS.ByteString -> IO ()
hpSendPatch hp payload = void (hpCall hp payload)

-- | Step the headless Rapier3D simulation forward by @dt@ seconds.
hpStep :: HeadlessPhysics -> Double -> IO StepResult
hpStep hp dt = do
  let req = Aeson.encode (Aeson.object ["type" .= ("step" :: Text), "dt" .= dt])
  line <- hpCall hp req
  case Aeson.eitherDecodeStrict line of
    Right res -> pure res
    Left err -> fail ("EventHorizon headless step decode error: " <> err)

-- | Capture a deterministic base64-encoded Rapier3D world snapshot.
hpTakeSnapshot :: HeadlessPhysics -> IO Text
hpTakeSnapshot hp = do
  let req = Aeson.encode (Aeson.object ["type" .= ("takeSnapshot" :: Text)])
  line <- hpCall hp req
  case Aeson.eitherDecodeStrict line of
    Right (Aeson.Object obj) ->
      case KM.lookup "data" obj of
        Just (Aeson.String txt) -> pure txt
        _ -> fail "EventHorizon headless takeSnapshot missing data field"
    other -> fail ("EventHorizon headless takeSnapshot decode error: " <> show other)

-- | Restore the headless Rapier3D world from a previously captured base64 snapshot.
hpRestoreSnapshot :: HeadlessPhysics -> Text -> IO (IntMap BodyState)
hpRestoreSnapshot hp b64 = do
  let req = Aeson.encode (Aeson.object ["type" .= ("restoreSnapshot" :: Text), "data" .= b64])
  line <- hpCall hp req
  case Aeson.eitherDecodeStrict line of
    Right val ->
      case Aeson.parseEither parseRestore val of
        Right delta -> pure delta
        Left err -> fail ("EventHorizon headless restoreSnapshot parse error: " <> err)
    Left err -> fail ("EventHorizon headless restoreSnapshot decode error: " <> err)
  where
    parseRestore = Aeson.withObject "RestoreOk" $ \o -> do
      rawDelta <- o .:? "snapshotDelta"
      parseSnapDeltaObj rawDelta

--------------------------------------------------------------------------------
-- Shared Deterministic Tick Pipeline
--------------------------------------------------------------------------------

worldToPhysicsScene :: WorldScene bid tid -> Scene bid tid
worldToPhysicsScene ws = combineScene ws defaultPlayerView

isPhysicsCmd :: Command bid -> Bool
isPhysicsCmd (PlaySound _) = False
isPhysicsCmd _ = True

extractPhysicsCmds :: [MultiCommand pid bid] -> [Command bid]
extractPhysicsCmds = mapMaybe go
  where
    go (WorldCmd c) | isPhysicsCmd c = Just c
    go (PlayerCmd _ c) | isPhysicsCmd c = Just c
    go _ = Nothing

extractAudioForPlayer :: Eq pid => pid -> [MultiCommand pid bid] -> [Command bid]
extractAudioForPlayer pid = mapMaybe go
  where
    go (WorldCmd c@(PlaySound _)) = Just c
    go (PlayerCmd p c@(PlaySound _)) | p == pid = Just c
    go _ = Nothing

mergeSnapDelta :: Ord bid => Double -> P.IdTable bid -> IntMap BodyState -> Snapshot bid -> Snapshot bid
mergeSnapDelta simT tbl delta prevSnap =
  let bodies' =
        IM.foldlWithKey'
          ( \acc wid bs ->
              case P.lookupWireId wid tbl of
                Just bid -> Map.insert bid bs acc
                Nothing -> acc
          )
          (snapBodies prevSnap)
          delta
   in Snapshot {snapTime = simT, snapBodies = bodies'}

resolveCollisions :: Ord bid => P.IdTable bid -> [P.RawEvent] -> [MultiEvent pid bid tid]
resolveCollisions tbl rawCols =
  let resolved = mapMaybe resolveOne rawCols
   in sortBy compareCol resolved
  where
    resolveOne (P.RawCollisionStart wa wb) = do
      a <- P.lookupWireId wa tbl
      b <- P.lookupWireId wb tbl
      let (ca, cb) = canonicalPair a b
      pure (WorldCollisionStart ca cb)
    resolveOne (P.RawCollisionEnd wa wb) = do
      a <- P.lookupWireId wa tbl
      b <- P.lookupWireId wb tbl
      let (ca, cb) = canonicalPair a b
      pure (WorldCollisionEnd ca cb)
    resolveOne _ = Nothing

    compareCol (WorldCollisionStart a1 b1) (WorldCollisionStart a2 b2) = compare (a1, b1) (a2, b2)
    compareCol (WorldCollisionStart _ _) _ = LT
    compareCol _ (WorldCollisionStart _ _) = GT
    compareCol (WorldCollisionEnd a1 b1) (WorldCollisionEnd a2 b2) = compare (a1, b1) (a2, b2)
    compareCol _ _ = EQ

syncWorldToPhysics
  :: (Ord bid, Ord tid)
  => HeadlessPhysics
  -> WorldScene bid tid
  -> [Command bid]
  -> DiffState bid tid
  -> P.WireState bid tid
  -> IO (DiffState bid tid, P.WireState bid tid)
syncWorldToPhysics hp ws physCmds diffSt wireSt = do
  let (patch, diffSt') = diffScene (Just diffSt) (worldToPhysicsScene ws)
      needsSend = case patch of
        PatchReset _ -> True
        PatchDelta d -> not (isEmptyDelta d) || not (null physCmds)
  advanceMemoGen
  if needsSend
    then do
      let (payload, wireSt') = P.encodePatch patch physCmds wireSt
      hpSendPatch hp payload
      pure (diffSt', wireSt')
    else pure (diffSt', wireSt)

-- | Execute one deterministic 60 Hz simulation tick shared identically across
-- 'playLocalMulti', 'serveMulti', and 'connectMulti' (both live prediction and
-- rollback replay).
stepDeterministicTick
  :: (Ord pid, Ord bid, Ord tid)
  => Game pid bid tid world
  -> HeadlessPhysics
  -> [MultiEvent pid bid tid]
  -> world
  -> Snapshot bid
  -> TickTimers tid
  -> DiffState bid tid
  -> P.WireState bid tid
  -> IO
       ( world
       , Snapshot bid
       , TickTimers tid
       , DiffState bid tid
       , P.WireState bid tid
       , [MultiCommand pid bid]
       )
stepDeterministicTick game hp incomingEvents world0 snap0 timers0 diff0 wire0 = do
  let (firedTids, timers1) = stepTickTimers timers0
      preEvents = incomingEvents <> map WorldTimerFired firedTids
      stepEv s (!w, accCmds) ev =
        let (!w', cs) = gameStep game ev s w
         in (w', accCmds <> cs)
      (!world1, cmds1) = foldl' (stepEv snap0) (world0, []) preEvents
      physCmds1 = extractPhysicsCmds cmds1
  (diff1, wire1) <- syncWorldToPhysics hp (gameWorld game world1) physCmds1 diff0 wire0

  stepRes <- hpStep hp (1.0 / fromIntegral ticksPerSecond)
  let snap1 =
        mergeSnapDelta
          (srTime stepRes)
          (P.wsBodies wire1)
          (srSnapshotDelta stepRes)
          (P.wsSnapshot wire1)
      wire1Snap = wire1 {P.wsSnapshot = snap1}
      colEvents = resolveCollisions (P.wsBodies wire1Snap) (srCollisions stepRes)
      (!world2, cmds2) = foldl' (stepEv snap1) (world1, []) colEvents
      physCmds2 = extractPhysicsCmds cmds2
  (diff2, wire2) <- syncWorldToPhysics hp (gameWorld game world2) physCmds2 diff1 wire1Snap

  let snap2 = P.wsSnapshot wire2
      timers2 = reconcileTickTimers (wsTimers (gameWorld game world2)) timers1
  pure (world2, snap2, timers2, diff2, wire2, cmds1 <> cmds2)

initHeadlessWorld
  :: (Ord bid, Ord tid)
  => HeadlessPhysics
  -> WorldScene bid tid
  -> IO (DiffState bid tid, P.WireState bid tid, Snapshot bid, TickTimers tid)
initHeadlessWorld hp ws = do
  let sc = worldToPhysicsScene ws
      (initPatch, initDiff) = diffScene Nothing sc
      (initPayload, initWire) = P.encodePatch initPatch [] P.emptyWireState
  hpSendPatch hp initPayload
  let initTimers = reconcileTickTimers (wsTimers ws) emptyTickTimers
  pure (initDiff, initWire, P.wsSnapshot initWire, initTimers)

--------------------------------------------------------------------------------
-- Browser WebSocket & HTTP Helpers
--------------------------------------------------------------------------------

loadStaticFile :: FilePath -> IO LBS.ByteString
loadStaticFile relPath = do
  localExists <- doesFileExist relPath
  if localExists
    then LBS.readFile relPath
    else do
      dataPath <- getDataFileName relPath
      LBS.readFile dataPath

httpHandler :: LBS.ByteString -> LBS.ByteString -> Wai.Application
httpHandler indexHtml bundleJs req respond =
  case Wai.pathInfo req of
    [] ->
      respond $
        Wai.responseLBS status200 [(hContentType, "text/html; charset=utf-8")] indexHtml
    ["index.html"] ->
      respond $
        Wai.responseLBS status200 [(hContentType, "text/html; charset=utf-8")] indexHtml
    ["bundle.js"] ->
      respond $
        Wai.responseLBS status200 [(hContentType, "application/javascript; charset=utf-8")] bundleJs
    _ ->
      respond $
        Wai.responseLBS status404 [(hContentType, "text/plain")] "Not Found"

encodeConfigMsg :: Text -> LBS.ByteString
encodeConfigMsg mode =
  Aeson.encode (Aeson.object ["type" .= ("config" :: Text), "mode" .= mode])

encodePosesMsg
  :: Ord bid
  => Bool
  -> P.IdTable bid
  -> Snapshot bid
  -> Map bid (Vec3, Quat)
  -> (Maybe LBS.ByteString, Map bid (Vec3, Quat))
encodePosesMsg smooth tbl snap prevPoses =
  let (pairs, nextPoses) =
        Map.foldlWithKey'
          ( \(acc, pm) bid bs ->
              case Map.lookup bid (P.idToWire tbl) of
                Nothing -> (acc, pm)
                Just wid ->
                  let cur = (bsPos bs, bsRot bs)
                   in case Map.lookup bid prevPoses of
                        Just old | not smooth && approxPose old cur -> (acc, pm)
                        _ ->
                          let val =
                                Aeson.object
                                  [ "pos" .= vec3ToVal (bsPos bs)
                                  , "rot" .= quatToVal (bsRot bs)
                                  ]
                           in ( (Key.fromString (show wid) .= val) : acc
                              , Map.insert bid cur pm
                              )
          )
          ([], prevPoses)
          (snapBodies snap)
   in if null pairs
        then (Nothing, nextPoses)
        else
          let msg =
                Aeson.encode $
                  Aeson.object
                    [ "type" .= ("poses" :: Text)
                    , "smooth" .= smooth
                    , "poses" .= Aeson.object pairs
                    ]
           in (Just msg, nextPoses)
  where
    approxPose (Vec3 x1 y1 z1, Quat qx1 qy1 qz1 qw1) (Vec3 x2 y2 z2, Quat qx2 qy2 qz2 qw2) =
      abs (x1 - x2) < 1e-4
        && abs (y1 - y2) < 1e-4
        && abs (z1 - z2) < 1e-4
        && abs (qx1 - qx2) < 1e-4
        && abs (qy1 - qy2) < 1e-4
        && abs (qz1 - qz2) < 1e-4
        && abs (qw1 - qw2) < 1e-4

resolveRawInput :: P.IdTable bid -> RawInput -> InputEvent bid
resolveRawInput tbl ri = case ri of
  RawInKeyDown k -> InKeyDown k
  RawInKeyUp k -> InKeyUp k
  RawInMouseDown b mHit ->
    let hit = do
          (wid, pt) <- mHit
          bid <- P.lookupWireId wid tbl
          pure (bid, pt)
     in InMouseDown b hit
  RawInMouseUp b -> InMouseUp b

rawEventsToInputs :: P.IdTable bid -> [P.RawEvent] -> [InputEvent bid]
rawEventsToInputs tbl = mapMaybe go
  where
    go (P.RawKeyDown k) = Just (InKeyDown k)
    go (P.RawKeyUp k) = Just (InKeyUp k)
    go (P.RawMouseDown b mHit) =
      let hit = do
            (wid, pt) <- mHit
            bid <- P.lookupWireId wid tbl
            pure (bid, pt)
       in Just (InMouseDown b hit)
    go (P.RawMouseUp b) = Just (InMouseUp b)
    go _ = Nothing

rawEventsToRawInputs :: [P.RawEvent] -> [RawInput]
rawEventsToRawInputs = mapMaybe go
  where
    go (P.RawKeyDown k) = Just (RawInKeyDown k)
    go (P.RawKeyUp k) = Just (RawInKeyUp k)
    go (P.RawMouseDown b mHit) = Just (RawInMouseDown b mHit)
    go (P.RawMouseUp b) = Just (RawInMouseUp b)
    go _ = Nothing

parsePlayerQuery :: BS.ByteString -> Maybe Int
parsePlayerQuery rawPath =
  let s = BS8.unpack rawPath
      (_, q) = break (== '?') s
   in case q of
        ('?' : rest) -> findParam (splitOn '&' rest)
        _ -> Nothing
  where
    splitOn _ [] = []
    splitOn c xs =
      let (a, b) = break (== c) xs
       in a : case b of
            [] -> []
            (_ : rest) -> splitOn c rest
    findParam [] = Nothing
    findParam (p : ps) =
      case stripPrefix "player=" p of
        Just v -> readMaybe v
        Nothing -> findParam ps

allPids :: (Bounded pid, Enum pid) => [pid]
allPids = [minBound .. maxBound]

indexToPid :: forall pid. (Bounded pid, Enum pid) => Int -> Maybe pid
indexToPid idx
  | idx >= fromEnum (minBound :: pid) && idx <= fromEnum (maxBound :: pid) = Just (toEnum idx)
  | otherwise = Nothing

sleepUntilTick :: Word64 -> IO Word64
sleepUntilTick targetNs = do
  nowNs <- getMonotonicTimeNSec
  let tickNs = 16666667 :: Word64
  if targetNs > nowNs
    then do
      let remUs = fromIntegral ((targetNs - nowNs) `div` 1000) :: Int
      when (remUs > 0) (threadDelay remUs)
      pure (targetNs + tickNs)
    else pure (nowNs + tickNs)

--------------------------------------------------------------------------------
-- 1. playLocalMulti: One Authoritative Server, Multiple Local Browser Tabs
--------------------------------------------------------------------------------

data BrowserClient pid bid tid = BrowserClient
  { bcPid :: !pid
  , bcConn :: !WS.Connection
  , bcDiff :: !(DiffState bid tid)
  , bcWire :: !(P.WireState bid tid)
  , bcLastPoses :: !(Map bid (Vec3, Quat))
  }

data LocalMultiState pid bid tid world = LocalMultiState
  { lmsWorld :: !world
  , lmsTick :: !Tick
  , lmsTimers :: !(TickTimers tid)
  , lmsSnap :: !(Snapshot bid)
  , lmsPhysDiff :: !(DiffState bid tid)
  , lmsPhysWire :: !(P.WireState bid tid)
  , lmsNextClientId :: !Int
  , lmsClients :: !(Map Int (BrowserClient pid bid tid))
  , lmsPendingEvents :: ![MultiEvent pid bid tid]
  }

-- | Run a multi-user game locally with one authoritative Haskell + headless
-- Rapier3D physics engine serving multiple browser tabs
-- (e.g. @http://localhost:3000/?player=1@ and @http://localhost:3000/?player=2@).
playLocalMulti
  :: (Ord pid, Ord bid, Ord tid, Show pid, Show bid, Show tid, Bounded pid, Enum pid)
  => Config
  -> Game pid bid tid world
  -> IO ()
playLocalMulti cfg game = do
  indexHtml <- loadStaticFile "static/index.html"
  bundleJs <- loadStaticFile "static/bundle.js"
  bracket startHeadlessPhysics stopHeadlessPhysics $ \hp -> do
    let initW = gameInit game
    (initDiff, initWire, initSnap, initTimers) <- initHeadlessWorld hp (gameWorld game initW)
    stateVar <-
      newMVar
        LocalMultiState
          { lmsWorld = initW
          , lmsTick = 0
          , lmsTimers = initTimers
          , lmsSnap = initSnap
          , lmsPhysDiff = initDiff
          , lmsPhysWire = initWire
          , lmsNextClientId = 1
          , lmsClients = Map.empty
          , lmsPendingEvents = []
          }

    now0 <- getMonotonicTimeNSec
    tickerTid <- forkIO (localTickerLoop game hp stateVar (now0 + 16666667))

    let port = cfgPort cfg
    putStrLn $ "EventHorizon Multi (Local): " <> T.unpack (cfgTitle cfg)
    putStrLn $ "  Player 1: http://localhost:" <> show port <> "/?player=1"
    putStrLn $ "  Player 2: http://localhost:" <> show port <> "/?player=2"
    putStrLn "  Press Ctrl+C to quit."
    hFlush stdout

    let app =
          WaiWS.websocketsOr
            WS.defaultConnectionOptions
            (localWsHandler cfg game stateVar)
            (httpHandler indexHtml bundleJs)
    Warp.run port app `finally` killThread tickerTid

localTickerLoop
  :: (Ord pid, Ord bid, Ord tid)
  => Game pid bid tid world
  -> HeadlessPhysics
  -> MVar (LocalMultiState pid bid tid world)
  -> Word64
  -> IO ()
localTickerLoop game hp stateVar nextTargetNs = do
  sends <-
    modifyMVar stateVar $ \st -> do
      let events = reverse (lmsPendingEvents st)
      if Map.null (lmsClients st) && null events
        then pure (st {lmsPendingEvents = []}, [])
        else do
          (world', snap', timers', physDiff', physWire', allCmds) <-
            stepDeterministicTick
              game
              hp
              events
              (lmsWorld st)
              (lmsSnap st)
              (lmsTimers st)
              (lmsPhysDiff st)
              (lmsPhysWire st)

          let ws' = gameWorld game world'
              (clients', payloads) =
                Map.foldlWithKey'
                  ( \(cmap, acc) cid bc ->
                      let pid = bcPid bc
                          sc = combineScene ws' (gameView game pid world')
                          (patch, diff') = diffScene (Just (bcDiff bc)) sc
                          audioCmds = extractAudioForPlayer pid allCmds
                          needPatch = case patch of
                            PatchReset _ -> True
                            PatchDelta d -> not (isEmptyDelta d) || not (null audioCmds)
                          (mPatchBytes, wire', prevPoses) =
                            if needPatch
                              then
                                let (bs, w') = P.encodePatch patch audioCmds (bcWire bc)
                                    resetPoses = case patch of
                                      PatchReset _ -> Map.empty
                                      PatchDelta _ -> bcLastPoses bc
                                 in (Just bs, w', resetPoses)
                              else (Nothing, bcWire bc, bcLastPoses bc)
                          (mPoseBytes, nextPoses) =
                            encodePosesMsg False (P.wsBodies wire') snap' prevPoses
                          bc' =
                            bc
                              { bcDiff = diff'
                              , bcWire = wire'
                              , bcLastPoses = nextPoses
                              }
                          msgs = catMaybes [mPatchBytes, mPoseBytes]
                       in ( Map.insert cid bc' cmap
                          , if null msgs then acc else (bcConn bc, msgs) : acc
                          )
                  )
                  (Map.empty, [])
                  (lmsClients st)

              st' =
                st
                  { lmsWorld = world'
                  , lmsTick = lmsTick st + 1
                  , lmsTimers = timers'
                  , lmsSnap = snap'
                  , lmsPhysDiff = physDiff'
                  , lmsPhysWire = physWire'
                  , lmsClients = clients'
                  , lmsPendingEvents = []
                  }
          pure (st', payloads)

  forM_ sends $ \(conn, msgs) ->
    forM_ msgs $ \m ->
      WS.sendTextData conn m `catch` (\(_ :: SomeException) -> pure ())

  nextTargetNs' <- sleepUntilTick nextTargetNs
  localTickerLoop game hp stateVar nextTargetNs'

localWsHandler
  :: forall pid bid tid world
   . (Ord pid, Ord bid, Ord tid, Show pid, Bounded pid, Enum pid)
  => Config
  -> Game pid bid tid world
  -> MVar (LocalMultiState pid bid tid world)
  -> WS.ServerApp
localWsHandler cfg game stateVar pending = do
  let reqPath = WS.requestPath (WS.pendingRequest pending)
      requestedNum = parsePlayerQuery reqPath
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (pure ()) $ do
    (cid, pid, initPayload, initPoseMsg) <-
      modifyMVar stateVar $ \st -> do
        let usedPids = map bcPid (Map.elems (lmsClients st))
            chosenPid = case requestedNum of
              Just n ->
                case indexToPid (n - 1) of
                  Just p -> p
                  Nothing -> pickFreePid usedPids
              Nothing -> pickFreePid usedPids
            sc = combineScene (gameWorld game (lmsWorld st)) (gameView game chosenPid (lmsWorld st))
            (initPatch, initDiff) = diffScene Nothing sc
            (payload, initWire) = P.encodePatch initPatch [] P.emptyWireState
            (mPose, poseCache) = encodePosesMsg False (P.wsBodies initWire) (lmsSnap st) Map.empty
            cidNew = lmsNextClientId st
            bc =
              BrowserClient
                { bcPid = chosenPid
                , bcConn = conn
                , bcDiff = initDiff
                , bcWire = initWire
                , bcLastPoses = poseCache
                }
            st' =
              st
                { lmsNextClientId = cidNew + 1
                , lmsClients = Map.insert cidNew bc (lmsClients st)
                , lmsPendingEvents = PlayerJoined chosenPid : lmsPendingEvents st
                }
        pure (st', (cidNew, chosenPid, payload, mPose))

    when (cfgDebug cfg) $
      putStrLn ("EventHorizon Multi: connected client #" <> show cid <> " as " <> show pid)

    WS.sendTextData conn (encodeConfigMsg "authoritative")
    WS.sendTextData conn initPayload
    forM_ initPoseMsg (WS.sendTextData conn)

    (readerLoop conn cid pid `catch` (\(_ :: SomeException) -> pure ()))
      `finally` cleanupClient cid pid
  where
    pickFreePid used =
      case filter (`notElem` used) (allPids :: [pid]) of
        (p : _) -> p
        [] -> minBound

    cleanupClient cid pid =
      modifyMVar_ stateVar $ \st -> do
        let clients' = Map.delete cid (lmsClients st)
            stillPresent = any ((== pid) . bcPid) (Map.elems clients')
            pending' =
              if stillPresent
                then lmsPendingEvents st
                else PlayerLeft pid : lmsPendingEvents st
        pure (st {lmsClients = clients', lmsPendingEvents = pending'})

    readerLoop c cid pid = do
      raw <- WS.receiveData c
      case P.decodeClientBatch raw of
        Left _ -> readerLoop c cid pid
        Right batch -> do
          modifyMVar_ stateVar $ \st ->
            case Map.lookup cid (lmsClients st) of
              Nothing -> pure st
              Just bc ->
                let inps = rawEventsToInputs (P.wsBodies (bcWire bc)) (P.rcbEvents batch)
                    evs = map (PlayerInput pid) inps
                 in pure (st {lmsPendingEvents = reverse evs <> lmsPendingEvents st})
          readerLoop c cid pid

--------------------------------------------------------------------------------
-- 2. serveMulti: Authoritative Networked Server (Input-Only Broadcasts)
--------------------------------------------------------------------------------

data ServerState pid bid tid world = ServerState
  { ssWorld :: !world
  , ssTick :: !Tick
  , ssTimers :: !(TickTimers tid)
  , ssSnap :: !(Snapshot bid)
  , ssPhysDiff :: !(DiffState bid tid)
  , ssPhysWire :: !(P.WireState bid tid)
  , ssPeers :: !(Map Int (pid, WS.Connection))
  , ssQueuedInputs :: !(Map Tick [(Int, RawInput)])
  , ssQueuedJoins :: !(Map Tick [Int])
  , ssQueuedLeaves :: !(Map Tick [Int])
  }

-- | Run an authoritative multiplayer server on @cfgPort cfg@ accepting
-- predicting 'connectMulti' Haskell clients on @/multi@.
--
-- Runs deterministic 60 Hz ticks with headless Rapier3D and broadcasts only
-- compact input frames ('NetTickInputs') plus periodic snapshot hashes
-- ('NetConfirm').
serveMulti
  :: (Ord pid, Ord bid, Ord tid, Show pid, Show bid, Show tid, Bounded pid, Enum pid)
  => Config
  -> Game pid bid tid world
  -> IO ()
serveMulti cfg game =
  bracket startHeadlessPhysics stopHeadlessPhysics $ \hp -> do
    let initW = gameInit game
    (initDiff, initWire, initSnap, initTimers) <- initHeadlessWorld hp (gameWorld game initW)
    stateVar <-
      newMVar
        ServerState
          { ssWorld = initW
          , ssTick = 0
          , ssTimers = initTimers
          , ssSnap = initSnap
          , ssPhysDiff = initDiff
          , ssPhysWire = initWire
          , ssPeers = Map.empty
          , ssQueuedInputs = Map.empty
          , ssQueuedJoins = Map.empty
          , ssQueuedLeaves = Map.empty
          }

    now0 <- getMonotonicTimeNSec
    tickerTid <- forkIO (serverTickerLoop game hp stateVar (now0 + 16666667))

    let port = cfgPort cfg
    putStrLn $ "EventHorizon Authoritative Server: " <> T.unpack (cfgTitle cfg)
    putStrLn $ "  Listening on ws://0.0.0.0:" <> show port <> "/multi"
    putStrLn "  Press Ctrl+C to quit."
    hFlush stdout

    let app =
          WaiWS.websocketsOr
            WS.defaultConnectionOptions
            (serverWsHandler cfg stateVar)
            (\_ respond -> respond (Wai.responseLBS status200 [(hContentType, "text/plain")] "EventHorizon Multi Server"))
    Warp.run port app `finally` killThread tickerTid

serverTickerLoop
  :: forall pid bid tid world
   . (Ord pid, Ord bid, Ord tid, Bounded pid, Enum pid)
  => Game pid bid tid world
  -> HeadlessPhysics
  -> MVar (ServerState pid bid tid world)
  -> Word64
  -> IO ()
serverTickerLoop game hp stateVar nextTargetNs = do
  (conns, msgs) <-
    modifyMVar stateVar $ \st -> do
      if Map.null (ssPeers st)
        then pure (st, ([], []))
        else do
          let t = ssTick st
              joins = Map.findWithDefault [] t (ssQueuedJoins st)
              leaves = Map.findWithDefault [] t (ssQueuedLeaves st)
              rawInps = sortBy (comparing fst) (Map.findWithDefault [] t (ssQueuedInputs st))
              bodyTbl = P.wsBodies (ssPhysWire st)
              joinEvs = [PlayerJoined p | idx <- joins, Just p <- [indexToPid idx :: Maybe pid]]
              leaveEvs = [PlayerLeft p | idx <- leaves, Just p <- [indexToPid idx :: Maybe pid]]
              inpEvs =
                [ PlayerInput p (resolveRawInput bodyTbl ri)
                | (idx, ri) <- rawInps
                , Just p <- [indexToPid idx :: Maybe pid]
                ]
              allEvs = joinEvs <> leaveEvs <> inpEvs

          (world', snap', timers', physDiff', physWire', _) <-
            stepDeterministicTick
              game
              hp
              allEvs
              (ssWorld st)
              (ssSnap st)
              (ssTimers st)
              (ssPhysDiff st)
              (ssPhysWire st)

          let tickInputMsg = Aeson.encode (NetTickInputs t rawInps)
              confirmMsgs =
                if t > 0 && t `mod` 60 == 0
                  then [Aeson.encode (NetConfirm t (hashSnapshot (ssSnap st)))]
                  else []
              peerConns = map snd (Map.elems (ssPeers st))
              st' =
                st
                  { ssWorld = world'
                  , ssTick = t + 1
                  , ssTimers = timers'
                  , ssSnap = snap'
                  , ssPhysDiff = physDiff'
                  , ssPhysWire = physWire'
                  , ssQueuedInputs = Map.delete t (ssQueuedInputs st)
                  , ssQueuedJoins = Map.delete t (ssQueuedJoins st)
                  , ssQueuedLeaves = Map.delete t (ssQueuedLeaves st)
                  }
          pure (st', (peerConns, tickInputMsg : confirmMsgs))

  forM_ conns $ \conn ->
    forM_ msgs $ \m ->
      WS.sendTextData conn m `catch` (\(_ :: SomeException) -> pure ())

  nextTargetNs' <- sleepUntilTick nextTargetNs
  serverTickerLoop game hp stateVar nextTargetNs'

serverWsHandler
  :: forall pid bid tid world
   . (Ord pid, Show pid, Bounded pid, Enum pid)
  => Config
  -> MVar (ServerState pid bid tid world)
  -> WS.ServerApp
serverWsHandler cfg stateVar pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (pure ()) $ do
    rawHello <- WS.receiveData conn
    case Aeson.eitherDecode rawHello of
      Right NetJoin -> do
        mAssigned <-
          modifyMVar stateVar $ \st -> do
            let usedIndices = Map.keys (ssPeers st)
                freePids = filter (\p -> fromEnum p `notElem` usedIndices) (allPids :: [pid])
            case freePids of
              [] -> pure (st, Nothing)
              (pid : _) -> do
                let pIdx = fromEnum pid
                    joinTick = ssTick st + 1
                    peers' = Map.insert pIdx (pid, conn) (ssPeers st)
                    joins' = Map.insertWith (\n o -> o <> n) joinTick [pIdx] (ssQueuedJoins st)
                    allConns = map snd (Map.elems peers')
                    st' = st {ssPeers = peers', ssQueuedJoins = joins'}
                pure (st', Just (pIdx, pid, ssTick st, joinTick, allConns))

        case mAssigned of
          Nothing -> WS.sendClose conn ("Server full" :: Text)
          Just (pIdx, pid, curTick, joinTick, allConns) -> do
            when (cfgDebug cfg) $
              putStrLn ("EventHorizon Server: player joined " <> show pid <> " at tick " <> show joinTick)
            WS.sendTextData conn (Aeson.encode (NetWelcome pIdx curTick))
            let joinBroadcast = Aeson.encode (NetPlayerJoined pIdx joinTick)
            forM_ allConns $ \c ->
              WS.sendTextData c joinBroadcast `catch` (\(_ :: SomeException) -> pure ())

            (peerLoop conn pIdx `catch` (\(_ :: SomeException) -> pure ()))
              `finally` disconnectPeer pIdx
      _ -> WS.sendClose conn ("Expected NetJoin" :: Text)
  where
    disconnectPeer pIdx = do
      (leaveTick, remainingConns) <-
        modifyMVar stateVar $ \st -> do
          let lt = ssTick st + 1
              peers' = Map.delete pIdx (ssPeers st)
              leaves' = Map.insertWith (\n o -> o <> n) lt [pIdx] (ssQueuedLeaves st)
              st' = st {ssPeers = peers', ssQueuedLeaves = leaves'}
          pure (st', (lt, map snd (Map.elems peers')))
      let leaveMsg = Aeson.encode (NetPlayerLeft pIdx leaveTick)
      forM_ remainingConns $ \c ->
        WS.sendTextData c leaveMsg `catch` (\(_ :: SomeException) -> pure ())

    peerLoop c pIdx = do
      raw <- WS.receiveData c
      case Aeson.eitherDecode raw of
        Right (NetClientInput clientTick ri) -> do
          modifyMVar_ stateVar $ \st -> do
            let targetTick = max (ssTick st) clientTick
                q' = Map.insertWith (\n o -> o <> n) targetTick [(pIdx, ri)] (ssQueuedInputs st)
            pure (st {ssQueuedInputs = q'})
          peerLoop c pIdx
        _ -> peerLoop c pIdx

--------------------------------------------------------------------------------
-- 3. connectMulti: Local Predicting Rollback Client + Browser UI
--------------------------------------------------------------------------------

data PhysBookmark bid tid = PhysBookmark
  { pbRapierB64 :: !Text
  , pbDiff :: !(DiffState bid tid)
  , pbWire :: !(P.WireState bid tid)
  }

data ClientNetEvent
  = CneTickInputs !Tick ![(Int, RawInput)]
  | CneConfirm !Tick !Word64
  | CnePlayerJoined !Int !Tick
  | CnePlayerLeft !Int !Tick
  | CnePhysicsSync !Tick !Text

data PredictClientState pid bid tid world = PredictClientState
  { pcsMyPid :: !pid
  , pcsWorld :: !world
  , pcsTick :: !Tick
  , pcsTimers :: !(TickTimers tid)
  , pcsSnap :: !(Snapshot bid)
  , pcsPhysDiff :: !(DiffState bid tid)
  , pcsPhysWire :: !(P.WireState bid tid)
  , pcsRollback :: !(RollbackState pid bid tid world)
  , pcsBookmarks :: !(Map Tick (PhysBookmark bid tid))
  , pcsConfirmedInputs :: !(Map Tick [(pid, InputEvent bid)])
  , pcsSchedJoins :: !(Map Tick [pid])
  , pcsSchedLeaves :: !(Map Tick [pid])
  , pcsLocalQueue :: ![RawInput]
  , pcsNetQueue :: ![ClientNetEvent]
  , pcsBrowser :: !(Maybe (BrowserClient pid bid tid))
  }

-- | Run a local predicting rollback client connected to a 'serveMulti' host.
--
-- Applies local player inputs immediately (0 ms local lag), tracks
-- 'RollbackState', rewinds and silently fast-forwards headless Rapier3D on
-- misprediction, deduplicates audio via 'dedupSounds', and notifies the local
-- browser UI to apply visual error smoothing.
connectMulti
  :: forall pid bid tid world
   . (Ord pid, Ord bid, Ord tid, Show pid, Show bid, Show tid, Bounded pid, Enum pid)
  => String
  -> Int
  -> Config
  -> Game pid bid tid world
  -> IO ()
connectMulti host port cfg game = do
  indexHtml <- loadStaticFile "static/index.html"
  bundleJs <- loadStaticFile "static/bundle.js"
  putStrLn $ "EventHorizon Predicting Client: connecting to ws://" <> host <> ":" <> show port <> "/multi ..."
  hFlush stdout
  WS.runClient host port "/multi" $ \srvConn -> do
    WS.sendTextData srvConn (Aeson.encode NetJoin)
    helloRaw <- WS.receiveData srvConn
    (myPid, startTick) <- case Aeson.eitherDecode helloRaw of
      Right (NetWelcome pIdx t) ->
        case indexToPid pIdx :: Maybe pid of
          Just p -> pure (p, t)
          Nothing -> fail ("EventHorizon: server assigned out-of-range player index " <> show pIdx)
      other -> fail ("EventHorizon: expected NetWelcome, got: " <> show other)

    bracket startHeadlessPhysics stopHeadlessPhysics $ \hp -> do
      let initW = gameInit game
      (initDiff, initWire, initSnap, initTimers) <- initHeadlessWorld hp (gameWorld game initW)
      initB64 <- hpTakeSnapshot hp
      let rb0 = (initRollback initW initSnap) {rsCurrentTick = startTick, rsConfirmedTick = startTick}
          bm0 = PhysBookmark initB64 initDiff initWire
      stateVar <-
        newMVar
          PredictClientState
            { pcsMyPid = myPid
            , pcsWorld = initW
            , pcsTick = startTick
            , pcsTimers = initTimers
            , pcsSnap = initSnap
            , pcsPhysDiff = initDiff
            , pcsPhysWire = initWire
            , pcsRollback = rb0
            , pcsBookmarks = Map.singleton startTick bm0
            , pcsConfirmedInputs = Map.empty
            , pcsSchedJoins = Map.empty
            , pcsSchedLeaves = Map.empty
            , pcsLocalQueue = []
            , pcsNetQueue = []
            , pcsBrowser = Nothing
            }

      recvTid <- forkIO (serverReceiverLoop srvConn stateVar)
      now0 <- getMonotonicTimeNSec
      tickTid <- forkIO (predictTickerLoop cfg game hp srvConn stateVar (now0 + 16666667))

      let localPort = cfgPort cfg
      putStrLn $ "  Assigned player: " <> show myPid <> " (start tick " <> show startTick <> ")"
      putStrLn $ "  Open http://localhost:" <> show localPort <> " in your browser to play."
      putStrLn "  Press Ctrl+C to quit."
      hFlush stdout

      let app =
            WaiWS.websocketsOr
              WS.defaultConnectionOptions
              (predictBrowserHandler game stateVar)
              (httpHandler indexHtml bundleJs)
      Warp.run localPort app
        `finally` (killThread tickTid >> killThread recvTid)

serverReceiverLoop
  :: WS.Connection
  -> MVar (PredictClientState pid bid tid world)
  -> IO ()
serverReceiverLoop srvConn stateVar =
  loop `catch` (\(_ :: SomeException) -> pure ())
  where
    loop = do
      raw <- WS.receiveData srvConn
      case Aeson.eitherDecode raw of
        Right (NetTickInputs t inps) ->
          enqueue (CneTickInputs t inps) >> loop
        Right (NetConfirm t h) ->
          enqueue (CneConfirm t h) >> loop
        Right (NetPlayerJoined pIdx t) ->
          enqueue (CnePlayerJoined pIdx t) >> loop
        Right (NetPlayerLeft pIdx t) ->
          enqueue (CnePlayerLeft pIdx t) >> loop
        Right (NetPhysicsSync t d) ->
          enqueue (CnePhysicsSync t d) >> loop
        _ -> loop

    enqueue ev =
      modifyMVar_ stateVar $ \st ->
        pure (st {pcsNetQueue = ev : pcsNetQueue st})

predictBrowserHandler
  :: (Ord bid, Ord tid)
  => Game pid bid tid world
  -> MVar (PredictClientState pid bid tid world)
  -> WS.ServerApp
predictBrowserHandler game stateVar pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (pure ()) $ do
    (initPayload, initPoseMsg) <-
      modifyMVar stateVar $ \st -> do
        let pid = pcsMyPid st
            sc = combineScene (gameWorld game (pcsWorld st)) (gameView game pid (pcsWorld st))
            (initPatch, initDiff) = diffScene Nothing sc
            (payload, initWire) = P.encodePatch initPatch [] P.emptyWireState
            (mPose, poseCache) = encodePosesMsg False (P.wsBodies initWire) (pcsSnap st) Map.empty
            bc =
              BrowserClient
                { bcPid = pid
                , bcConn = conn
                , bcDiff = initDiff
                , bcWire = initWire
                , bcLastPoses = poseCache
                }
        pure (st {pcsBrowser = Just bc}, (payload, mPose))

    WS.sendTextData conn (encodeConfigMsg "authoritative")
    WS.sendTextData conn initPayload
    forM_ initPoseMsg (WS.sendTextData conn)

    (readerLoop conn `catch` (\(_ :: SomeException) -> pure ()))
      `finally` modifyMVar_ stateVar (\st -> pure (st {pcsBrowser = Nothing}))
  where
    readerLoop c = do
      raw <- WS.receiveData c
      case P.decodeClientBatch raw of
        Left _ -> readerLoop c
        Right batch -> do
          let rawInps = rawEventsToRawInputs (P.rcbEvents batch)
          unless (null rawInps) $
            modifyMVar_ stateVar $ \st ->
              pure (st {pcsLocalQueue = reverse rawInps <> pcsLocalQueue st})
          readerLoop c

predictTickerLoop
  :: forall pid bid tid world
   . (Ord pid, Ord bid, Ord tid, Show pid, Bounded pid, Enum pid)
  => Config
  -> Game pid bid tid world
  -> HeadlessPhysics
  -> WS.Connection
  -> MVar (PredictClientState pid bid tid world)
  -> Word64
  -> IO ()
predictTickerLoop cfg game hp srvConn stateVar nextTargetNs = do
  (outToServer, outToBrowser) <-
    modifyMVar stateVar $ \st0 -> do
      -- 1. Process incoming authoritative server messages (reconciliation / rollback)
      let netEvs = reverse (pcsNetQueue st0)
      (st1, didRollback, replayAudio) <-
        foldM (applyNetEvent cfg game hp) (st0 {pcsNetQueue = []}, False, []) netEvs

      -- 2. Drain local inputs, prepare to send to server & predict immediately (0 ms lag)
      let curTick = pcsTick st1
          localRaw = reverse (pcsLocalQueue st1)
          myPid = pcsMyPid st1
          bodyTbl = P.wsBodies (pcsPhysWire st1)
          localTyped = map (resolveRawInput bodyTbl) localRaw
          serverMsgs = [Aeson.encode (NetClientInput curTick ri) | ri <- localRaw]
          rb1 = foldl' (\r inp -> recordLocalInput curTick myPid inp r) (pcsRollback st1) localTyped

      -- 3. Snapshot physics state before stepping curTick
      snapB64 <- hpTakeSnapshot hp
      let bm = PhysBookmark snapB64 (pcsPhysDiff st1) (pcsPhysWire st1)
          bms1 = trimBookmarks (Map.insert curTick bm (pcsBookmarks st1))
          tickInputs = [(myPid, inp) | inp <- localTyped]
          rb2 = recordHistory curTick (pcsWorld st1) (pcsSnap st1) (pcsTimers st1) tickInputs rb1
          joinEvs = map PlayerJoined (Map.findWithDefault [] curTick (pcsSchedJoins st1))
          leaveEvs = map PlayerLeft (Map.findWithDefault [] curTick (pcsSchedLeaves st1))
          allEvs = joinEvs <> leaveEvs <> map (uncurry PlayerInput) tickInputs

      -- 4. Step local prediction deterministically
      (world', snap', timers', physDiff', physWire', cmds) <-
        stepDeterministicTick
          game
          hp
          allEvs
          (pcsWorld st1)
          (pcsSnap st1)
          (pcsTimers st1)
          (pcsPhysDiff st1)
          (pcsPhysWire st1)

      let (dedupedCmds, rb3) = dedupSounds curTick cmds rb2
          audioCmds = replayAudio <> extractAudioForPlayer myPid dedupedCmds

      -- 5. Diff and push visual update + smooth pose correction to local browser
      let (browser', browserPayloads) =
            case pcsBrowser st1 of
              Nothing -> (Nothing, [])
              Just bc ->
                let sc = combineScene (gameWorld game world') (gameView game myPid world')
                    (patch, diff') = diffScene (Just (bcDiff bc)) sc
                    needPatch = case patch of
                      PatchReset _ -> True
                      PatchDelta d -> not (isEmptyDelta d) || not (null audioCmds)
                    (mPatchBytes, wire', prevPoses) =
                      if needPatch
                        then
                          let (bs, w') = P.encodePatch patch audioCmds (bcWire bc)
                              resetPoses = case patch of
                                PatchReset _ -> Map.empty
                                PatchDelta _ -> bcLastPoses bc
                           in (Just bs, w', resetPoses)
                        else (Nothing, bcWire bc, bcLastPoses bc)
                    (mPoseBytes, nextPoses) =
                      encodePosesMsg didRollback (P.wsBodies wire') snap' prevPoses
                    bc' =
                      bc
                        { bcDiff = diff'
                        , bcWire = wire'
                        , bcLastPoses = nextPoses
                        }
                 in (Just bc', [(bcConn bc, catMaybes [mPatchBytes, mPoseBytes])])

          stFinal =
            st1
              { pcsWorld = world'
              , pcsTick = curTick + 1
              , pcsTimers = timers'
              , pcsSnap = snap'
              , pcsPhysDiff = physDiff'
              , pcsPhysWire = physWire'
              , pcsRollback = rb3
              , pcsBookmarks = bms1
              , pcsLocalQueue = []
              , pcsBrowser = browser'
              }
      pure (stFinal, (serverMsgs, browserPayloads))

  forM_ outToServer $ \m ->
    WS.sendTextData srvConn m `catch` (\(_ :: SomeException) -> pure ())

  forM_ outToBrowser $ \(conn, msgs) ->
    forM_ msgs $ \m ->
      WS.sendTextData conn m `catch` (\(_ :: SomeException) -> pure ())

  nextTargetNs' <- sleepUntilTick nextTargetNs
  predictTickerLoop cfg game hp srvConn stateVar nextTargetNs'

trimBookmarks :: Map Tick a -> Map Tick a
trimBookmarks m
  | Map.size m <= maxHistoryTicks = m
  | otherwise = trimBookmarks (Map.deleteMin m)

applyNetEvent
  :: forall pid bid tid world
   . (Ord pid, Ord bid, Ord tid, Bounded pid, Enum pid)
  => Config
  -> Game pid bid tid world
  -> HeadlessPhysics
  -> (PredictClientState pid bid tid world, Bool, [Command bid])
  -> ClientNetEvent
  -> IO (PredictClientState pid bid tid world, Bool, [Command bid])
applyNetEvent cfg game hp (st, didRb, audioAcc) ev = case ev of
  CnePlayerJoined pIdx t ->
    case indexToPid pIdx :: Maybe pid of
      Nothing -> pure (st, didRb, audioAcc)
      Just p ->
        let js' = Map.insertWith (\n o -> o <> n) t [p] (pcsSchedJoins st)
         in pure (st {pcsSchedJoins = js'}, didRb, audioAcc)
  CnePlayerLeft pIdx t ->
    case indexToPid pIdx :: Maybe pid of
      Nothing -> pure (st, didRb, audioAcc)
      Just p ->
        let ls' = Map.insertWith (\n o -> o <> n) t [p] (pcsSchedLeaves st)
         in pure (st {pcsSchedLeaves = ls'}, didRb, audioAcc)
  CnePhysicsSync _t b64 -> do
    snapDelta <- hpRestoreSnapshot hp b64
    let snap' = mergeSnapDelta (snapTime (pcsSnap st)) (P.wsBodies (pcsPhysWire st)) snapDelta (pcsSnap st)
    pure (st {pcsSnap = snap'}, True, audioAcc)
  CneTickInputs t rawInps -> do
    let tbl = maybe (P.wsBodies (pcsPhysWire st)) (P.wsBodies . pbWire) (Map.lookup t (pcsBookmarks st))
        typedInps =
          [ (p, resolveRawInput tbl ri)
          | (idx, ri) <- rawInps
          , Just p <- [indexToPid idx :: Maybe pid]
          ]
        stWithConf = st {pcsConfirmedInputs = Map.insert t typedInps (pcsConfirmedInputs st)}
    reconcileAt cfg game hp t typedInps Nothing (stWithConf, didRb, audioAcc)
  CneConfirm t serverHash -> do
    let confirmed = Map.findWithDefault [] t (pcsConfirmedInputs st)
    reconcileAt cfg game hp t confirmed (Just serverHash) (st, didRb, audioAcc)

reconcileAt
  :: (Ord pid, Ord bid, Ord tid)
  => Config
  -> Game pid bid tid world
  -> HeadlessPhysics
  -> Tick
  -> [(pid, InputEvent bid)]
  -> Maybe Word64
  -> (PredictClientState pid bid tid world, Bool, [Command bid])
  -> IO (PredictClientState pid bid tid world, Bool, [Command bid])
reconcileAt cfg game hp t authInputs mHash (st, didRb, audioAcc) =
  case confirmServerTick t authInputs mHash (pcsRollback st) of
    ReconcileOk rb' ->
      let bms' = Map.filterWithKey (\k _ -> k >= t) (pcsBookmarks st)
          conf' = Map.filterWithKey (\k _ -> k >= t) (pcsConfirmedInputs st)
       in pure (st {pcsRollback = rb', pcsBookmarks = bms', pcsConfirmedInputs = conf'}, didRb, audioAcc)
    ReconcileRollback rewindTick frame schedule rbRewound ->
      case Map.lookup rewindTick (pcsBookmarks st) of
        Nothing ->
          pure (st {pcsRollback = rbRewound}, didRb, audioAcc)
        Just bm -> do
          when (cfgDebug cfg) $
            putStrLn
              ( "EventHorizon Rollback: tick "
                  <> show rewindTick
                  <> " -> replaying "
                  <> show (length schedule)
                  <> " ticks"
              )
          void (hpRestoreSnapshot hp (pbRapierB64 bm))
          let initAcc =
                ( hfWorld frame
                , hfSnapshot frame
                , hfTimers frame
                , pbDiff bm
                , pbWire bm
                , rbRewound
                , Map.filterWithKey (\k _ -> k < rewindTick) (pcsBookmarks st)
                , audioAcc
                )
          (w', s', tm', d', pw', rb', bms', audio') <-
            foldM (replayOne st) initAcc schedule
          pure
            ( st
                { pcsWorld = w'
                , pcsSnap = s'
                , pcsTimers = tm'
                , pcsPhysDiff = d'
                , pcsPhysWire = pw'
                , pcsRollback = rb'
                , pcsBookmarks = bms'
                }
            , True
            , audio'
            )
  where
    replayOne stRef (w, s, tm, d, pw, rb, bms, aud) (repTick, inps) = do
      b64 <- hpTakeSnapshot hp
      let bm = PhysBookmark b64 d pw
          bms' = trimBookmarks (Map.insert repTick bm bms)
          rb1 = recordHistory repTick w s tm inps rb
          joins = map PlayerJoined (Map.findWithDefault [] repTick (pcsSchedJoins stRef))
          leaves = map PlayerLeft (Map.findWithDefault [] repTick (pcsSchedLeaves stRef))
          evs = joins <> leaves <> map (uncurry PlayerInput) inps
      (w', s', tm', d', pw', cmds) <-
        stepDeterministicTick game hp evs w s tm d pw
      let (deduped, rb2) = dedupSounds repTick cmds rb1
          newAudio = extractAudioForPlayer (pcsMyPid stRef) deduped
      pure (w', s', tm', d', pw', rb2, bms', aud <> newAudio)
