{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (evaluate)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Game.EventHorizon
import Game.EventHorizon.Diff
import Game.EventHorizon.Memo (advanceMemoGen, clearMemo)
import Game.EventHorizon.Protocol
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Text.Printf (printf)
import Text.Read (readMaybe)
import YoursCraft

main :: IO ()
main = do
  args <- getArgs
  if "--bench" `elem` args
    then runStressBenchmark
    else do
      let radius = parseFlagInt "--radius" 18 args
          port = parseFlagInt "--port" 3003 args
          noMemo = "--no-memo" `elem` args
          st0 = (initialVoxelWorld radius) {vsUseMemo = not noMemo}
      playWith
        (defaultConfig "YoursCraft — 3D Voxel Builder & Diff Stress Test") {cfgPort = port}
        st0
        renderVoxel
        stepVoxel

parseFlagInt :: String -> Int -> [String] -> Int
parseFlagInt _ def [] = def
parseFlagInt flag def (x : y : rest)
  | x == flag = maybe def id (readMaybe y)
  | otherwise = parseFlagInt flag def (y : rest)
parseFlagInt _ def [_] = def

-- | Compare 3 strategies on realistic YoursCraft place/break edits across
-- ~1,700 / ~10,000 / ~40,000-block worlds:
--   1. No diffing (full scene rebuild + full JSON encode on every click)
--   2. Naive flat diff (unmemoized bodies list + diffScene + delta JSON encode)
--   3. Memoized chunked SceneTree diff (mapTree chunkNode + diffScene + delta JSON encode)
runStressBenchmark :: IO ()
runStressBenchmark = do
  putStrLn "YoursCraft Real-World Diffing Stress Test (alternating place/break edits across distinct chunks)"
  putStrLn (replicate 126 '-')
  mapM_ benchRadius [18, 45, 95]
  putStrLn (replicate 126 '-')
  where
    diffSteps :: Int
    diffSteps = 50

    benchRadius :: Int -> IO ()
    benchRadius rad = do
      let st0 = initialVoxelWorld rad
          nBlocks = vsBlockCount st0
          nChunks = Map.size (vsChunks st0)
          -- Use fewer iterations for the slow full-JSON no-diff baseline on huge worlds
          noDiffSteps = if nBlocks > 15000 then 4 else if nBlocks > 5000 then 10 else 25
          edits =
            [ (even i, (x, 10 + (k `mod` 3), z))
            | i <- [0 .. diffSteps - 1]
            , let k = i `div` 2
                  x = ((k * 19) `mod` (2 * rad - 2)) - (rad - 1)
                  z = ((k * 31) `mod` (2 * rad - 2)) - (rad - 1)
            ]

      -- 1. No diffing (full PatchReset JSON on every edit)
      let runNoDiff (!st, !totalBytes) (isPlace, coord) = do
            let !st' = if isPlace then placeBlockAt coord Brick st else breakBlockAt coord st
                !sc = renderVoxelFlat st'
                (!bytes, _) = encodePatch (PatchReset sc) [] emptyWireState
                !len = LBS.length bytes
            _ <- evaluate len
            pure (st', totalBytes + len)
      t0 <- getCPUTime
      (_, noDiffBytes) <- foldM' runNoDiff (st0, 0) (take noDiffSteps edits)
      t1 <- getCPUTime

      -- 2. Naive flat diff (force full initial render + reset BEFORE timing incremental steps)
      let scFlat0 = renderVoxelFlat st0
          (pFlat0, dsFlat0) = diffScene Nothing scFlat0
          (initFlatBytes, wsFlat0) = encodePatch pFlat0 [] emptyWireState
      _ <- evaluate (LBS.length initFlatBytes)
      let runFlatDiff (!st, !ds, !ws, !totalBytes) (isPlace, coord) = do
            let !st' = if isPlace then placeBlockAt coord Brick st else breakBlockAt coord st
                !sc = renderVoxelFlat st'
                (!patch, !ds') = diffScene (Just ds) sc
                (!bytes, !ws') = encodePatch patch [] ws
                !len = LBS.length bytes
            _ <- evaluate len
            pure (st', ds', ws', totalBytes + len)
      t2 <- getCPUTime
      (_, _, _, flatBytes) <- foldM' runFlatDiff (st0, dsFlat0, wsFlat0, 0) edits
      t3 <- getCPUTime

      -- 3. Memoized chunked SceneTree diff (force full initial render + reset BEFORE timing)
      clearMemo
      let scMemo0 = renderVoxelMemo st0
          (pMemo0, dsMemo0) = diffScene Nothing scMemo0
          (initMemoBytes, wsMemo0) = encodePatch pMemo0 [] emptyWireState
      _ <- evaluate (LBS.length initMemoBytes)
      advanceMemoGen
      let runMemoDiff (!st, !ds, !ws, !totalBytes) (isPlace, coord) = do
            let !st' = if isPlace then placeBlockAt coord Brick st else breakBlockAt coord st
                !sc = renderVoxelMemo st'
                (!patch, !ds') = diffScene (Just ds) sc
                (!bytes, !ws') = encodePatch patch [] ws
                !len = LBS.length bytes
            _ <- evaluate len
            advanceMemoGen
            pure (st', ds', ws', totalBytes + len)
      t4 <- getCPUTime
      (_, _, _, memoBytes) <- foldM' runMemoDiff (st0, dsMemo0, wsMemo0, 0) edits
      t5 <- getCPUTime

      let msNoDiff = fromIntegral (t1 - t0) / 1e9 / fromIntegral noDiffSteps :: Double
          msFlat = fromIntegral (t3 - t2) / 1e9 / fromIntegral diffSteps :: Double
          msMemo = fromIntegral (t5 - t4) / 1e9 / fromIntegral diffSteps :: Double
          kbNoDiff = fromIntegral noDiffBytes / fromIntegral noDiffSteps / 1024.0 :: Double
          bFlat = fromIntegral flatBytes / fromIntegral diffSteps :: Double
          bMemo = fromIntegral memoBytes / fromIntegral diffSteps :: Double
          speedupNoDiff = msNoDiff / max 1e-6 msMemo
          speedupFlat = msFlat / max 1e-6 msMemo
      printf
        "Blocks=%5d (%3d chunks) | No-Diff: %7.2f ms (%7.1f KB) | Flat-Diff: %6.2f ms (%4.0f B) | Memo-Tree: %6.3f ms (%4.0f B) | %7.0fx vs No-Diff, %6.1fx vs Flat\n"
        nBlocks
        nChunks
        msNoDiff
        kbNoDiff
        msFlat
        bFlat
        msMemo
        bMemo
        speedupNoDiff
        speedupFlat
      hFlush stdout

foldM' :: Monad m => (s -> a -> m s) -> s -> [a] -> m s
foldM' _ !s [] = pure s
foldM' f !s (x : xs) = do
  !s' <- f s x
  foldM' f s' xs
