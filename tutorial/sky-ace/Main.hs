{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Step01_FirstFlight as Step1
import qualified Step02_Controls as Step2
import qualified Step03_ForestAndCrash as Step3
import qualified Step04_ShootingAndTimers as Step4
import qualified Step05_TargetsAndMemo as Step5
import qualified Step06_CommandsAndAudio as Step6
import qualified Step07_Multiplayer as Step7
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Just (stepNum, port) -> runStep stepNum port
    Nothing -> putStrLn usageText

parseArgs :: [String] -> Maybe (Int, Int)
parseArgs [stepStr] = do
  n <- readMaybe stepStr
  if n >= 1 && n <= 7 then Just (n, 3000) else Nothing
parseArgs [stepStr, "--port", portStr] = do
  n <- readMaybe stepStr
  p <- readMaybe portStr
  if n >= 1 && n <= 7 then Just (n, p) else Nothing
parseArgs _ = Nothing

runStep :: Int -> Int -> IO ()
runStep n port = case n of
  1 -> Step1.run port
  2 -> Step2.run port
  3 -> Step3.run port
  4 -> Step4.run port
  5 -> Step5.run port
  6 -> Step6.run port
  7 -> Step7.runLocal port
  _ -> putStrLn usageText

usageText :: String
usageText =
  unlines
    [ "Sky Ace Incremental Tutorial — EventHorizon"
    , ""
    , "Usage: cabal run sky-ace-tutorial -- <STEP> [--port N]"
    , ""
    , "Steps:"
    , "  1  First Flight              (zero state, FlightDrive, FollowCamera)"
    , "  2  Event-Driven Controls     (Set Key, KeyDown/KeyUp steering & boost)"
    , "  3  Forest, Collisions & R    (staticWorldTree, matchPair, Crashed, sceneEpoch)"
    , "  4  Shooting & Timers         (Snapshot, vector operators, Once timers, CollisionFilter)"
    , "  5  Targets & Memoized Tree   (AI waves, Every timers, mapTree & intMapTree)"
    , "  6  Commands & Web Audio      (playWith, PlaySound sweep/noise, HemisphereLight, Material)"
    , "  7  2-Player Multiplayer      (Game pid bid tid world, renderWorld/renderView, Teleport respawn)"
    , "     (For Step 7, open http://localhost:3000/?player=1 and http://localhost:3000/?player=2)"
    , ""
    , "Read the companion walkthrough in tutorial/sky-ace/TUTORIAL.md"
    ]
