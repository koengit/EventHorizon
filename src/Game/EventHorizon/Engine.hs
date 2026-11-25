{-# LANGUAGE OverloadedStrings #-}

module Game.EventHorizon.Engine
  ( Config (..)
  , defaultConfig
  , play
  , playWith
  ) where

import Control.Exception (SomeException, catch)
import qualified Data.ByteString.Lazy as LBS
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import Game.EventHorizon.Diff
import Game.EventHorizon.Memo (advanceMemoGen)
import Game.EventHorizon.Protocol
import Game.EventHorizon.Scene (Scene)
import Game.EventHorizon.Types (Command, Event, Snapshot)
import Network.HTTP.Types (hContentType, status200, status404)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS
import Paths_event_horizon (getDataFileName)
import System.Directory (doesFileExist)
import System.IO (hFlush, stdout)

-- | Runtime configuration for 'play' and 'playWith'.
data Config = Config
  { cfgTitle :: !Text
  , cfgPort :: !Int
  , cfgDebug :: !Bool
  }
  deriving (Eq, Show)

defaultConfig :: Text -> Config
defaultConfig title =
  Config
    { cfgTitle = title
    , cfgPort = 3000
    , cfgDebug = False
    }

-- | Run a pure event-driven 3D game.
--
-- The update function ONLY runs when an 'Event' occurs (player input,
-- physics collision/sensor event, or a game-scheduled timer).
play
  :: (Ord bid, Ord tid, Show bid, Show tid)
  => Config
  -> world
  -> (world -> Scene bid tid)
  -> (Event bid tid -> Snapshot bid -> world -> world)
  -> IO ()
play cfg initialWorld toScene onEvent =
  playWith cfg initialWorld toScene (\ev snap w -> (onEvent ev snap w, []))

-- | Like 'play', but the pure update function may also emit one-shot 'Command's
-- (physics impulses, velocity overrides, teleports, and procedural audio).
playWith
  :: (Ord bid, Ord tid, Show bid, Show tid)
  => Config
  -> world
  -> (world -> Scene bid tid)
  -> (Event bid tid -> Snapshot bid -> world -> (world, [Command bid]))
  -> IO ()
playWith cfg initialWorld toScene onEvent = do
  indexHtml <- loadStaticFile "static/index.html"
  bundleJs <- loadStaticFile "static/bundle.js"
  let port = cfgPort cfg
  putStrLn $ "EventHorizon: " <> T.unpack (cfgTitle cfg)
  putStrLn $ "  Open http://localhost:" <> show port <> " in your browser to play."
  putStrLn "  Press Ctrl+C to quit."
  hFlush stdout
  let app =
        WaiWS.websocketsOr
          WS.defaultConnectionOptions
          (wsHandler cfg initialWorld toScene onEvent)
          (httpHandler indexHtml bundleJs)
  Warp.run port app

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

wsHandler
  :: (Ord bid, Ord tid, Show bid, Show tid)
  => Config
  -> world
  -> (world -> Scene bid tid)
  -> (Event bid tid -> Snapshot bid -> world -> (world, [Command bid]))
  -> WS.ServerApp
wsHandler cfg initialWorld toScene onEvent pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (pure ()) $ do
    let initScene = toScene initialWorld
        (!initPatch, !initDiff) = diffScene Nothing initScene
        (initPayload, initWire) = encodePatch initPatch [] emptyWireState
    WS.sendTextData conn initPayload
    advanceMemoGen
    loop conn initialWorld initDiff initWire `catch` handleDisconnect
  where
    handleDisconnect :: SomeException -> IO ()
    handleDisconnect _ = pure ()

    loop conn world diffSt wireSt = do
      raw <- WS.receiveData conn
      case decodeClientBatch raw of
        Left err -> do
          if cfgDebug cfg
            then putStrLn ("EventHorizon protocol error: " <> err)
            else pure ()
          loop conn world diffSt wireSt
        Right batch ->
          case resolveClientBatch batch wireSt of
            Nothing ->
              -- Stale epoch event; silently discard.
              loop conn world diffSt wireSt
            Just (evs, snap, wireSt1) -> do
              if cfgDebug cfg && not (null evs)
                then putStrLn ("EventHorizon events: " <> show evs)
                else pure ()
              let stepEvent (!w, accCmds) ev =
                    let (!w', cmds) = onEvent ev snap w
                     in (w', accCmds <> cmds)
                  (!world', allCmds) = foldl' stepEvent (world, []) evs
                  (!patch, !diffSt') = diffScene (Just diffSt) (toScene world')
                  !shouldSend = case patch of
                    PatchReset _ -> True
                    PatchDelta d -> not (isEmptyDelta d) || not (null allCmds)
              if shouldSend
                then do
                  let (payload, wireSt2) = encodePatch patch allCmds wireSt1
                  WS.sendTextData conn payload
                  advanceMemoGen
                  loop conn world' diffSt' wireSt2
                else do
                  advanceMemoGen
                  loop conn world' diffSt' wireSt1
