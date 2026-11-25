{-# LANGUAGE OverloadedStrings #-}

module Main where

import SkyAce (skyAceMultiGame, initialGame, renderGame, stepGame)
import Game.EventHorizon
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Read (readMaybe)

data CliMode
  = ModeSingle !Int
  | ModeLocalMulti !Int
  | ModeServer !Int
  | ModeConnect !String !Int !Int
  deriving (Eq, Show)

parseHostPort :: String -> Maybe (String, Int)
parseHostPort s =
  case break (== ':') s of
    (h, ':' : pStr) | not (null h) -> do
      p <- readMaybe pStr
      pure (h, p)
    _ -> Nothing

parseArgs :: [String] -> Either String CliMode
parseArgs args = case args of
  [] -> Right (ModeSingle 3000)
  ("--port" : pStr : rest) -> do
    p <- parsePort pStr
    parseWithPort p rest
  ("--local-multi" : rest) -> do
    p <- parseOptionalPort 3000 rest
    Right (ModeLocalMulti p)
  ("--server" : rest) -> do
    p <- parseOptionalPort 4000 rest
    Right (ModeServer p)
  ("--connect" : hp : rest) ->
    case parseHostPort hp of
      Nothing -> Left ("Invalid HOST:PORT argument: " <> hp)
      Just (h, srvPort) -> do
        localPort <- parseOptionalPort 3000 rest
        Right (ModeConnect h srvPort localPort)
  other -> Left ("Unrecognized arguments: " <> unwords other)
  where
    parsePort s =
      case readMaybe s of
        Just p | p > 0 -> Right p
        _ -> Left ("Invalid port number: " <> s)

    parseOptionalPort def [] = Right def
    parseOptionalPort _ ["--port", pStr] = parsePort pStr
    parseOptionalPort _ extra = Left ("Unexpected arguments: " <> unwords extra)

    parseWithPort p [] = Right (ModeSingle p)
    parseWithPort p ["--local-multi"] = Right (ModeLocalMulti p)
    parseWithPort p ["--server"] = Right (ModeServer p)
    parseWithPort p ["--connect", hp] =
      case parseHostPort hp of
        Nothing -> Left ("Invalid HOST:PORT argument: " <> hp)
        Just (h, srvPort) -> Right (ModeConnect h srvPort p)
    parseWithPort _ extra = Left ("Unexpected arguments: " <> unwords extra)

usage :: String
usage =
  unlines
    [ "Usage: sky-ace [MODE] [--port N]"
    , "  (default)              Single-player Sky Ace on port 3000"
    , "  --local-multi          2-player local multi-tab server (/?player=1 & /?player=2)"
    , "  --server               Authoritative multiplayer server (default port 4000)"
    , "  --connect HOST:PORT    Predicting rollback client connecting to HOST:PORT (local browser port 3000)"
    ]

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err -> do
      putStrLn ("Error: " <> err)
      putStrLn usage
      exitFailure
    Right mode ->
      case mode of
        ModeSingle port ->
          playWith
            (defaultConfig "Sky Ace — 3D Flight & Dogfight") {cfgPort = port}
            initialGame
            renderGame
            stepGame
        ModeLocalMulti port ->
          playLocalMulti
            (defaultConfig "Sky Ace — 2P Local Dogfight") {cfgPort = port}
            skyAceMultiGame
        ModeServer port ->
          serveMulti
            (defaultConfig "Sky Ace — Authoritative Server") {cfgPort = port}
            skyAceMultiGame
        ModeConnect host srvPort localPort ->
          connectMulti
            host
            srvPort
            (defaultConfig "Sky Ace — 2P Rollback Client") {cfgPort = localPort}
            skyAceMultiGame
