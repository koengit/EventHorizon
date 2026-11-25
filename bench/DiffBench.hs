-- | Standalone benchmark comparing legacy naive flat-list Map diffing against
-- incremental SceneTree + memo ('intMapTree' and 'mapTree') for N = 1,000,
-- 10,000, and 50,000 bodies with delta = 1 mutation per step across 50 steps.
module Main where

import Control.Exception (evaluate)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Game.EventHorizon
import Game.EventHorizon.Diff
import Game.EventHorizon.Memo (advanceMemoGen, clearMemo)
import System.CPUTime (getCPUTime)
import System.IO (hFlush, stdout)
import Text.Printf (printf)

numSteps :: Int
numSteps = 50

-- | Realistic entity renderer (2-part mesh + box collider + pose).
mkEntityBody :: Int -> Double -> Body Int
mkEntityBody eid yPos =
  let x = fromIntegral (eid `mod` 250) * 3.0
      z = fromIntegral (eid `div` 250) * 3.0
      mesh =
        group
          [ colored blue (box (Vec3 1.2 1.2 1.2))
          , colored gold (translate (Vec3 0 0.8 0) (sphere 0.4))
          ]
   in (dynamicBody eid (at (Vec3 x yPos z)) (BoxCollider (Vec3 1.2 1.2 1.2)) mesh)
        { bodyMass = 1.5
        }

-- | Top-level CAF renderer passed to 'intMapTree' and 'mapTree'.
renderEntityNode :: Int -> Double -> SceneTree Int
renderEntityNode eid yPos = bodyNode (mkEntityBody eid yPos)

-- | Legacy flat-list Map diffing (simulates rebuilding [Body], indexing into a
-- Map, and structurally diffing all N entries on every event).
naiveDiffStep
  :: Map Int (Body Int)
  -> IntMap Double
  -> (Int, Int, Int, Map Int (Body Int))
naiveDiffStep oldMap world =
  let flatList = [mkEntityBody k v | (k, v) <- IM.toList world]
      newMap = Map.fromList [(bodyId b, b) | b <- flatList]
      remCount =
        Map.foldlWithKey'
          (\ !acc k _ -> if Map.member k newMap then acc else acc + 1)
          0
          oldMap
      (addCount, updCount) =
        Map.foldlWithKey' step (0, 0) newMap
   in (remCount, addCount, updCount, newMap)
  where
    step (!aAcc, !uAcc) k newB =
      case Map.lookup k oldMap of
        Nothing -> (aAcc + 1, uAcc)
        Just oldB ->
          if oldB == newB
            then (aAcc, uAcc)
            else (aAcc, uAcc + 1)

forceDelta :: ([Int], [Body Int], [BodyUpdate Int]) -> Int
forceDelta (rems, adds, upds) =
  length rems + length adds + foldl' (\ !acc u -> buId u `seq` buPose u `seq` acc + 1) 0 upds

benchNaive :: Int -> IO (Double, Int)
benchNaive n = do
  let !world0 = IM.fromList [(i, 1.0) | i <- [0 .. n - 1]]
      !map0 = Map.fromList [(i, mkEntityBody i 1.0) | i <- [0 .. n - 1]]
  _ <- evaluate (Map.size map0)
  tStart <- getCPUTime
  totalUpd <- go 1 world0 map0 0
  tEnd <- getCPUTime
  let elapsedMs = fromIntegral (tEnd - tStart) / 1.0e9
  pure (elapsedMs, totalUpd)
  where
    go !step !w !prevMap !acc
      | step > numSteps = pure acc
      | otherwise = do
          let !k = (step * 997) `mod` n
              !w' = IM.insert k (fromIntegral step + 0.5) w
              (!r, !a, !u, !nextMap) = naiveDiffStep prevMap w'
              !deltaCount = r + a + u
          _ <- evaluate deltaCount
          go (step + 1) w' nextMap (acc + deltaCount)

benchIntMapTree :: Int -> IO (Double, Int)
benchIntMapTree n = do
  clearMemo
  let !world0 = IM.fromList [(i, 1.0) | i <- [0 .. n - 1]]
  t0 <- evaluate (intMapTree renderEntityNode world0)
  advanceMemoGen
  tStart <- getCPUTime
  totalUpd <- go 1 world0 t0 0
  tEnd <- getCPUTime
  let elapsedMs = fromIntegral (tEnd - tStart) / 1.0e9
  pure (elapsedMs, totalUpd)
  where
    go !step !w !prevTree !acc
      | step > numSteps = pure acc
      | otherwise = do
          let !k = (step * 997) `mod` n
              !w' = IM.insert k (fromIntegral step + 0.5) w
          nextTree <- evaluate (intMapTree renderEntityNode w')
          deltaCount <- evaluate (forceDelta (diffTree prevTree nextTree))
          advanceMemoGen
          go (step + 1) w' nextTree (acc + deltaCount)

benchMapTree :: Int -> IO (Double, Int)
benchMapTree n = do
  clearMemo
  let !world0 = Map.fromList [(i, 1.0) | i <- [0 .. n - 1]]
  t0 <- evaluate (mapTree renderEntityNode world0)
  advanceMemoGen
  tStart <- getCPUTime
  totalUpd <- go 1 world0 t0 0
  tEnd <- getCPUTime
  let elapsedMs = fromIntegral (tEnd - tStart) / 1.0e9
  pure (elapsedMs, totalUpd)
  where
    go !step !w !prevTree !acc
      | step > numSteps = pure acc
      | otherwise = do
          let !k = (step * 997) `mod` n
              !w' = Map.insert k (fromIntegral step + 0.5) w
          nextTree <- evaluate (mapTree renderEntityNode w')
          deltaCount <- evaluate (forceDelta (diffTree prevTree nextTree))
          advanceMemoGen
          go (step + 1) w' nextTree (acc + deltaCount)

runSize :: Int -> IO ()
runSize n = do
  (naiveMs, naiveCnt) <- benchNaive n
  (intMapMs, intMapCnt) <- benchIntMapTree n
  (mapMs, mapCnt) <- benchMapTree n
  let stepsD = fromIntegral numSteps :: Double
      naiveStep = naiveMs / stepsD
      intMapStep = intMapMs / stepsD
      mapStep = mapMs / stepsD
      intMapSpeedup = if intMapMs > 0 then naiveMs / intMapMs else 0.0
      mapSpeedup = if mapMs > 0 then naiveMs / mapMs else 0.0
  printf
    "N = %6d | Naive: %8.3f ms/step | intMapTree: %7.4f ms/step (%7.1fx) | mapTree: %7.4f ms/step (%7.1fx) | deltas: (%d,%d,%d)\n"
    n
    naiveStep
    intMapStep
    intMapSpeedup
    mapStep
    mapSpeedup
    naiveCnt
    intMapCnt
    mapCnt
  hFlush stdout

main :: IO ()
main = do
  putStrLn "EventHorizon SceneTree + Memo Incremental Diff Benchmark"
  putStrLn ("Workload: delta = 1 non-local mutation per step across " <> show numSteps <> " steps")
  putStrLn (replicate 120 '-')
  mapM_ runSize [1000, 10000, 50000]
  putStrLn (replicate 120 '-')
  putStrLn "Done."
