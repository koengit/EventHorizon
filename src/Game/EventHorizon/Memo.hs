-- | Transparent pointer-equality memoization for pure scene rendering.
--
-- 'memo' and 'memo2' cache pure function results keyed by the heap identity
-- ('StableName') of the function and its argument(s). When an immutable input
-- (such as an untouched 'Data.Map.Strict.Map' or 'Data.IntMap.Strict.IntMap'
-- subtree, or a game-state record field) keeps the same heap pointer across
-- steps, the cached output is returned at its exact previous heap address in
-- O(1), allowing 'Game.EventHorizon.Diff.diffTree' to skip the entire subtree via
-- pointer equality.
--
-- Memory is bounded automatically via a 2-generation hot cache (@mcCurr@ /
-- @mcPrev@) flipped by 'advanceMemoGen' after every scene diff, backed by a
-- weak-referenced cold cache keyed on the live inputs so deep persistent-tree
-- subtrees remain hot across arbitrary mutation patterns without retaining
-- garbage-collected inputs.
--
-- Lennart would be proud :-)
module Game.EventHorizon.Memo
  ( memo
  , memo2
  , memo3
  , advanceMemoGen
  , clearMemo
  ) where

import Control.Exception (evaluate)
import Control.Monad (filterM, when)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Maybe (isJust)
import GHC.Exts (Any)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.StableName (StableName, hashStableName, makeStableName)
import System.Mem.Weak (Weak, deRefWeak, mkWeak)
import Unsafe.Coerce (unsafeCoerce)

data MemoEntry
  = MemoEntry1 !(StableName Any) !(StableName Any) !Any
  | MemoEntry2 !(StableName Any) !(StableName Any) !(StableName Any) !Any
  | MemoEntry3 !(StableName Any) !(StableName Any) !(StableName Any) !(StableName Any) !Any

data WeakMemoEntry
  = WeakEntry1 !(StableName Any) !(StableName Any) !(Weak ()) !(Weak Any)
  | WeakEntry2 !(StableName Any) !(StableName Any) !(StableName Any) !(Weak ()) !(Weak ()) !(Weak Any)
  | WeakEntry3 !(StableName Any) !(StableName Any) !(StableName Any) !(StableName Any) !(Weak ()) !(Weak ()) !(Weak ()) !(Weak Any)

data MemoCache = MemoCache
  { mcPrev :: !(IntMap [MemoEntry])
  , mcCurr :: !(IntMap [MemoEntry])
  , mcCurrSize :: !Int
  , mcWeak :: !(IntMap [WeakMemoEntry])
  , mcWeakSize :: !Int
  , mcWeakLimit :: !Int
  }

maxCurrEntries :: Int
maxCurrEntries = 250000

initialWeakLimit :: Int
initialWeakLimit = 250000

hardWeakLimit :: Int
hardWeakLimit = 500000

emptyMemoCache :: MemoCache
emptyMemoCache =
  MemoCache
    { mcPrev = IM.empty
    , mcCurr = IM.empty
    , mcCurrSize = 0
    , mcWeak = IM.empty
    , mcWeakSize = 0
    , mcWeakLimit = initialWeakLimit
    }

{-# NOINLINE globalMemoCache #-}
globalMemoCache :: IORef MemoCache
globalMemoCache = unsafePerformIO (newIORef emptyMemoCache)

-- | Flip the 1-step memo generations (@mcPrev := mcCurr; mcCurr := empty@).
-- Called by the engine after each scene diff finishes. Runs in O(1) in the
-- common case, and prunes dead weak entries only when the weak table exceeds
-- its size threshold.
advanceMemoGen :: IO ()
advanceMemoGen = do
  needPrune <- atomicModifyIORef' globalMemoCache $ \c ->
    let c' =
          c
            { mcPrev = mcCurr c
            , mcCurr = IM.empty
            , mcCurrSize = 0
            }
     in (c', mcWeakSize c' > mcWeakLimit c')
  when needPrune pruneWeakCache

-- | Clear all memoized entries (hot and weak caches).
clearMemo :: IO ()
clearMemo =
  atomicModifyIORef' globalMemoCache (\_ -> (emptyMemoCache, ()))

-- | Transparent pointer-equality memoization for a 1-argument pure function.
--
-- Evaluates the argument to WHNF before taking its 'StableName'. On a cache
-- hit, returns the exact previous heap pointer without re-running @f@.
{-# NOINLINE memo #-}
memo :: (a -> b) -> (a -> b)
memo f = \x -> unsafePerformIO (memoCall1 f x)

-- | Transparent pointer-equality memoization for a 2-argument pure function.
--
-- Evaluates both arguments to WHNF before taking their 'StableName's. Ideal
-- for functions taking an environment plus a persistent container subtree
-- (see 'Game.EventHorizon.Scene.mapTreeWith' and 'Game.EventHorizon.Scene.intMapTreeWith').
{-# NOINLINE memo2 #-}
memo2 :: (a -> b -> c) -> (a -> b -> c)
memo2 f x y = unsafePerformIO (memoCall2 f x y)

-- | Transparent pointer-equality memoization for a 3-argument pure function.
{-# NOINLINE memo3 #-}
memo3 :: (a -> b -> c -> d) -> (a -> b -> c -> d)
memo3 f x y z = unsafePerformIO (memoCall3 f x y z)

memoCall1 :: (a -> b) -> a -> IO b
memoCall1 f x = do
  f' <- evaluate f
  x' <- evaluate x
  snF <- makeStableName (unsafeCoerce f' :: Any)
  snX <- makeStableName (unsafeCoerce x' :: Any)
  let !h = hashStableName snF * 31 + hashStableName snX
  cache <- readIORef globalMemoCache
  case lookupEntry1 snF snX h (mcCurr cache) of
    Just val -> pure (unsafeCoerce val)
    Nothing ->
      case lookupEntry1 snF snX h (mcPrev cache) of
        Just val -> do
          promoteEntry h (MemoEntry1 snF snX val)
          pure (unsafeCoerce val)
        Nothing -> do
          mWeak <- lookupWeak1 snF snX h (mcWeak cache)
          case mWeak of
            Just val -> do
              promoteEntry h (MemoEntry1 snF snX val)
              pure (unsafeCoerce val)
            Nothing -> do
              res <- evaluate (f' x')
              let val = unsafeCoerce res :: Any
              wf <- mkWeak f' () Nothing
              wx <- mkWeak x' val Nothing
              insertFresh h (MemoEntry1 snF snX val) (WeakEntry1 snF snX wf wx)
              pure res

memoCall2 :: (a -> b -> c) -> a -> b -> IO c
memoCall2 f x y = do
  f' <- evaluate f
  x' <- evaluate x
  y' <- evaluate y
  snF <- makeStableName (unsafeCoerce f' :: Any)
  snX <- makeStableName (unsafeCoerce x' :: Any)
  snY <- makeStableName (unsafeCoerce y' :: Any)
  let !h = (hashStableName snF * 31 + hashStableName snX) * 31 + hashStableName snY
  cache <- readIORef globalMemoCache
  case lookupEntry2 snF snX snY h (mcCurr cache) of
    Just val -> pure (unsafeCoerce val)
    Nothing ->
      case lookupEntry2 snF snX snY h (mcPrev cache) of
        Just val -> do
          promoteEntry h (MemoEntry2 snF snX snY val)
          pure (unsafeCoerce val)
        Nothing -> do
          mWeak <- lookupWeak2 snF snX snY h (mcWeak cache)
          case mWeak of
            Just val -> do
              promoteEntry h (MemoEntry2 snF snX snY val)
              pure (unsafeCoerce val)
            Nothing -> do
              res <- evaluate (f' x' y')
              let val = unsafeCoerce res :: Any
              wf <- mkWeak f' () Nothing
              wx <- mkWeak x' () Nothing
              wy <- mkWeak y' val Nothing
              insertFresh
                h
                (MemoEntry2 snF snX snY val)
                (WeakEntry2 snF snX snY wf wx wy)
              pure res

memoCall3 :: (a -> b -> c -> d) -> a -> b -> c -> IO d
memoCall3 f x y z = do
  f' <- evaluate f
  x' <- evaluate x
  y' <- evaluate y
  z' <- evaluate z
  snF <- makeStableName (unsafeCoerce f' :: Any)
  snX <- makeStableName (unsafeCoerce x' :: Any)
  snY <- makeStableName (unsafeCoerce y' :: Any)
  snZ <- makeStableName (unsafeCoerce z' :: Any)
  let !h =
        ((hashStableName snF * 31 + hashStableName snX) * 31 + hashStableName snY) * 31
          + hashStableName snZ
  cache <- readIORef globalMemoCache
  case lookupEntry3 snF snX snY snZ h (mcCurr cache) of
    Just val -> pure (unsafeCoerce val)
    Nothing ->
      case lookupEntry3 snF snX snY snZ h (mcPrev cache) of
        Just val -> do
          promoteEntry h (MemoEntry3 snF snX snY snZ val)
          pure (unsafeCoerce val)
        Nothing -> do
          mWeak <- lookupWeak3 snF snX snY snZ h (mcWeak cache)
          case mWeak of
            Just val -> do
              promoteEntry h (MemoEntry3 snF snX snY snZ val)
              pure (unsafeCoerce val)
            Nothing -> do
              res <- evaluate (f' x' y' z')
              let val = unsafeCoerce res :: Any
              wf <- mkWeak f' () Nothing
              wx <- mkWeak x' () Nothing
              wy <- mkWeak y' () Nothing
              wz <- mkWeak z' val Nothing
              insertFresh
                h
                (MemoEntry3 snF snX snY snZ val)
                (WeakEntry3 snF snX snY snZ wf wx wy wz)
              pure res

lookupEntry1 :: StableName Any -> StableName Any -> Int -> IntMap [MemoEntry] -> Maybe Any
lookupEntry1 snF snX h m =
  case IM.lookup h m of
    Nothing -> Nothing
    Just entries -> go entries
  where
    go [] = Nothing
    go (MemoEntry1 ef ex val : rest)
      | ef == snF && ex == snX = Just val
      | otherwise = go rest
    go (_ : rest) = go rest

lookupEntry2
  :: StableName Any
  -> StableName Any
  -> StableName Any
  -> Int
  -> IntMap [MemoEntry]
  -> Maybe Any
lookupEntry2 snF snX snY h m =
  case IM.lookup h m of
    Nothing -> Nothing
    Just entries -> go entries
  where
    go [] = Nothing
    go (MemoEntry2 ef ex ey val : rest)
      | ef == snF && ex == snX && ey == snY = Just val
      | otherwise = go rest
    go (_ : rest) = go rest

lookupEntry3
  :: StableName Any
  -> StableName Any
  -> StableName Any
  -> StableName Any
  -> Int
  -> IntMap [MemoEntry]
  -> Maybe Any
lookupEntry3 snF snX snY snZ h m =
  case IM.lookup h m of
    Nothing -> Nothing
    Just entries -> go entries
  where
    go [] = Nothing
    go (MemoEntry3 ef ex ey ez val : rest)
      | ef == snF && ex == snX && ey == snY && ez == snZ = Just val
      | otherwise = go rest
    go (_ : rest) = go rest

lookupWeak1 :: StableName Any -> StableName Any -> Int -> IntMap [WeakMemoEntry] -> IO (Maybe Any)
lookupWeak1 snF snX h m =
  case IM.lookup h m of
    Nothing -> pure Nothing
    Just entries -> go entries
  where
    go [] = pure Nothing
    go (WeakEntry1 ef ex _ wx : rest)
      | ef == snF && ex == snX = do
          mVal <- deRefWeak wx
          case mVal of
            Just v -> pure (Just v)
            Nothing -> go rest
      | otherwise = go rest
    go (_ : rest) = go rest

lookupWeak2
  :: StableName Any
  -> StableName Any
  -> StableName Any
  -> Int
  -> IntMap [WeakMemoEntry]
  -> IO (Maybe Any)
lookupWeak2 snF snX snY h m =
  case IM.lookup h m of
    Nothing -> pure Nothing
    Just entries -> go entries
  where
    go [] = pure Nothing
    go (WeakEntry2 ef ex ey _ _ wy : rest)
      | ef == snF && ex == snX && ey == snY = do
          mVal <- deRefWeak wy
          case mVal of
            Just v -> pure (Just v)
            Nothing -> go rest
      | otherwise = go rest
    go (_ : rest) = go rest

lookupWeak3
  :: StableName Any
  -> StableName Any
  -> StableName Any
  -> StableName Any
  -> Int
  -> IntMap [WeakMemoEntry]
  -> IO (Maybe Any)
lookupWeak3 snF snX snY snZ h m =
  case IM.lookup h m of
    Nothing -> pure Nothing
    Just entries -> go entries
  where
    go [] = pure Nothing
    go (WeakEntry3 ef ex ey ez _ _ _ wz : rest)
      | ef == snF && ex == snX && ey == snY && ez == snZ = do
          mVal <- deRefWeak wz
          case mVal of
            Just v -> pure (Just v)
            Nothing -> go rest
      | otherwise = go rest
    go (_ : rest) = go rest

prependBucket :: Int -> a -> IntMap [a] -> IntMap [a]
prependBucket h entry = IM.alter (Just . maybe [entry] (entry :)) h

promoteEntry :: Int -> MemoEntry -> IO ()
promoteEntry !h !entry =
  atomicModifyIORef' globalMemoCache $ \c ->
    if mcCurrSize c >= maxCurrEntries
      then
        ( c
            { mcPrev = IM.empty
            , mcCurr = IM.singleton h [entry]
            , mcCurrSize = 1
            }
        , ()
        )
      else
        ( c
            { mcCurr = prependBucket h entry (mcCurr c)
            , mcCurrSize = mcCurrSize c + 1
            }
        , ()
        )

insertFresh :: Int -> MemoEntry -> WeakMemoEntry -> IO ()
insertFresh !h !entry !wEntry = do
  needPrune <- atomicModifyIORef' globalMemoCache $ \c ->
    let !wMap = prependBucket h wEntry (mcWeak c)
        !wSize = mcWeakSize c + 1
        c' =
          if mcCurrSize c >= maxCurrEntries
            then
              c
                { mcPrev = IM.empty
                , mcCurr = IM.singleton h [entry]
                , mcCurrSize = 1
                , mcWeak = wMap
                , mcWeakSize = wSize
                }
            else
              c
                { mcCurr = prependBucket h entry (mcCurr c)
                , mcCurrSize = mcCurrSize c + 1
                , mcWeak = wMap
                , mcWeakSize = wSize
                }
     in (c', wSize > mcWeakLimit c')
  when needPrune pruneWeakCache

isWeakAlive :: WeakMemoEntry -> IO Bool
isWeakAlive (WeakEntry1 _ _ wf wx) = do
  af <- isJust <$> deRefWeak wf
  if not af
    then pure False
    else isJust <$> deRefWeak wx
isWeakAlive (WeakEntry2 _ _ _ wf wx wy) = do
  af <- isJust <$> deRefWeak wf
  if not af
    then pure False
    else do
      ax <- isJust <$> deRefWeak wx
      if not ax
        then pure False
        else isJust <$> deRefWeak wy
isWeakAlive (WeakEntry3 _ _ _ _ wf wx wy wz) = do
  af <- isJust <$> deRefWeak wf
  if not af
    then pure False
    else do
      ax <- isJust <$> deRefWeak wx
      if not ax
        then pure False
        else do
          ay <- isJust <$> deRefWeak wy
          if not ay
            then pure False
            else isJust <$> deRefWeak wz

pruneWeakCache :: IO ()
pruneWeakCache = do
  c0 <- readIORef globalMemoCache
  when (mcWeakSize c0 > mcWeakLimit c0) $ do
    (pruned, liveCount) <- filterWeakMap (mcWeak c0)
    atomicModifyIORef' globalMemoCache $ \c ->
      if liveCount > hardWeakLimit
        then
          ( c
              { mcWeak = IM.empty
              , mcWeakSize = 0
              , mcWeakLimit = initialWeakLimit
              }
          , ()
          )
        else
          ( c
              { mcWeak = pruned
              , mcWeakSize = liveCount
              , mcWeakLimit = max initialWeakLimit (liveCount * 2)
              }
          , ()
          )

filterWeakMap :: IntMap [WeakMemoEntry] -> IO (IntMap [WeakMemoEntry], Int)
filterWeakMap m = go IM.empty 0 (IM.toList m)
  where
    go !acc !cnt [] = pure (acc, cnt)
    go !acc !cnt ((k, entries) : rest) = do
      alive <- filterM isWeakAlive entries
      case alive of
        [] -> go acc cnt rest
        xs ->
          let !n = length xs
           in go (IM.insert k xs acc) (cnt + n) rest
