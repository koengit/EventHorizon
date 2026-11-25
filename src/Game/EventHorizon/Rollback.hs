-- | Pure, deterministic rollback & client-prediction reconciliation engine.
--
-- Because EventHorizon worlds are pure immutable Haskell values, saving a history
-- frame at each tick is O(1) structural sharing. When a delayed remote input or
-- snapshot hash mismatch arrives from the authoritative server,
-- 'confirmServerTick' rewinds to the mispredicted frame and produces the exact
-- replay schedule needed to fast-forward back to the present tick.
module Game.EventHorizon.Rollback
  ( -- * Deterministic Integer-Tick Timers
    TimerMode (..)
  , TickTimerState (..)
  , TickTimers (..)
  , emptyTickTimers
  , secondsToTicks
  , reconcileTickTimers
  , stepTickTimers

    -- * History & Rollback Ring Buffer
  , maxHistoryTicks
  , HistoryFrame (..)
  , RollbackState (..)
  , initRollback
  , recordLocalInput
  , recordHistory

    -- * Reconciliation
  , ReconcileResult (..)
  , confirmServerTick

    -- * Deterministic Snapshot Hashing & Sound Deduplication
  , hashSnapshot
  , dedupSounds
  ) where

import Data.Bits (shiftR, xor, (.&.))
import Data.Int (Int64)
import Data.List (foldl', sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word64)
import Game.EventHorizon.Math
import Game.EventHorizon.Multi
import Game.EventHorizon.Scene (Timer (..))
import Game.EventHorizon.Types

-- | Mode of a deterministic tick timer.
data TimerMode
  = TimerOnce
  | TimerEvery
  deriving (Eq, Ord, Show)

-- | Integer-tick countdown state for a single active timer.
data TickTimerState = TickTimerState
  { ttsMode :: !TimerMode
  , ttsIntervalTicks :: !Int
  , ttsRemainingTicks :: !Int
  , ttsFired :: !Bool
  }
  deriving (Eq, Show)

-- | Map of active deterministic timers, stepped in integer ticks to eliminate
-- floating-point drift across peers.
newtype TickTimers tid = TickTimers
  { unTickTimers :: Map tid TickTimerState
  }
  deriving (Eq, Show)

emptyTickTimers :: TickTimers tid
emptyTickTimers = TickTimers Map.empty

-- | Convert a timer duration in seconds into a strictly positive integer tick count.
secondsToTicks :: Double -> Int
secondsToTicks sec = max 1 (round (sec * fromIntegral ticksPerSecond))

-- | Reconcile the running 'TickTimers' against the declarative timer list from
-- the current 'WorldScene'. Timers omitted from the list are cancelled; newly
-- added timers start with a full countdown; existing timers retain their
-- remaining countdown unless their mode or interval changed.
reconcileTickTimers :: Ord tid => [Timer tid] -> TickTimers tid -> TickTimers tid
reconcileTickTimers timers (TickTimers cur) =
  TickTimers (foldl' insertTimer Map.empty timers)
  where
    insertTimer acc t =
      let (tid, mode, interval) = case t of
            Once k sec -> (k, TimerOnce, secondsToTicks sec)
            Every k sec -> (k, TimerEvery, secondsToTicks sec)
          nextState = case Map.lookup tid cur of
            Nothing ->
              TickTimerState
                { ttsMode = mode
                , ttsIntervalTicks = interval
                , ttsRemainingTicks = interval
                , ttsFired = False
                }
            Just prev
              | ttsMode prev == mode && ttsIntervalTicks prev == interval -> prev
              | otherwise ->
                  TickTimerState
                    { ttsMode = mode
                    , ttsIntervalTicks = interval
                    , ttsRemainingTicks = min (ttsRemainingTicks prev) interval
                    , ttsFired = mode == TimerOnce && ttsFired prev
                    }
       in Map.insert tid nextState acc

-- | Advance all active timers by 1 tick. Returns the canonically sorted list
-- of expired timer IDs and the updated 'TickTimers'.
stepTickTimers :: Ord tid => TickTimers tid -> ([tid], TickTimers tid)
stepTickTimers (TickTimers m) =
  let (revFired, nextMap) =
        foldl' stepOne ([], Map.empty) (Map.toAscList m)
   in (reverse revFired, TickTimers nextMap)
  where
    stepOne (firedAcc, mapAcc) (tid, st)
      | ttsMode st == TimerOnce && ttsFired st =
          (firedAcc, Map.insert tid st mapAcc)
      | otherwise =
          let remTicks = ttsRemainingTicks st - 1
           in if remTicks <= 0
                then
                  let st' = case ttsMode st of
                        TimerOnce -> st {ttsRemainingTicks = 0, ttsFired = True}
                        TimerEvery -> st {ttsRemainingTicks = ttsIntervalTicks st}
                   in (tid : firedAcc, Map.insert tid st' mapAcc)
                else
                  (firedAcc, Map.insert tid (st {ttsRemainingTicks = remTicks}) mapAcc)

-- | Maximum number of history frames retained in the rollback ring buffer
-- (180 ticks = 3 seconds at 60 Hz).
maxHistoryTicks :: Int
maxHistoryTicks = 180

-- | Pure snapshot of all simulation state entering tick 'hfTick', together with
-- the inputs applied during that tick.
data HistoryFrame pid bid tid world = HistoryFrame
  { hfTick :: !Tick
  , hfWorld :: !world
  , hfSnapshot :: !(Snapshot bid)
  , hfTimers :: !(TickTimers tid)
  , hfInputs :: ![(pid, InputEvent bid)]
  }
  deriving (Eq, Show)

-- | Client rollback & prediction state.
data RollbackState pid bid tid world = RollbackState
  { rsCurrentTick :: !Tick
  , rsConfirmedTick :: !Tick
  , rsHistory :: !(Map Tick (HistoryFrame pid bid tid world))
  , rsLocalInputs :: !(Map Tick [(pid, InputEvent bid)])
  , rsPlayedSounds :: !(Set (Tick, Int))
  }
  deriving (Eq, Show)

-- | Create an initial 'RollbackState' seeded with the starting world and snapshot.
initRollback :: world -> Snapshot bid -> RollbackState pid bid tid world
initRollback initWorld initSnap =
  RollbackState
    { rsCurrentTick = 0
    , rsConfirmedTick = 0
    , rsHistory =
        Map.singleton
          0
          (HistoryFrame 0 initWorld initSnap emptyTickTimers [])
    , rsLocalInputs = Map.empty
    , rsPlayedSounds = Set.empty
    }

-- | Record a locally predicted input at a given tick so it will be re-applied
-- during any future rollback replay covering that tick.
recordLocalInput
  :: Tick
  -> pid
  -> InputEvent bid
  -> RollbackState pid bid tid world
  -> RollbackState pid bid tid world
recordLocalInput tick pid inp rs =
  rs
    { rsLocalInputs =
        Map.insertWith (\new old -> old <> new) tick [(pid, inp)] (rsLocalInputs rs)
    }

-- | Record a history frame for @tick@ and cap the ring buffer to 'maxHistoryTicks'.
recordHistory
  :: Tick
  -> world
  -> Snapshot bid
  -> TickTimers tid
  -> [(pid, InputEvent bid)]
  -> RollbackState pid bid tid world
  -> RollbackState pid bid tid world
recordHistory tick world snap timers inputs rs =
  let frame =
        HistoryFrame
          { hfTick = tick
          , hfWorld = world
          , hfSnapshot = snap
          , hfTimers = timers
          , hfInputs = inputs
          }
      hist1 = Map.insert tick frame (rsHistory rs)
      hist2 = trimHistory hist1
      curTick' = max (rsCurrentTick rs) tick
   in rs
        { rsCurrentTick = curTick'
        , rsHistory = hist2
        }

trimHistory :: Map Tick a -> Map Tick a
trimHistory m
  | Map.size m <= maxHistoryTicks = m
  | otherwise = trimHistory (Map.deleteMin m)

-- | Result of reconciling local prediction against an authoritative server tick.
data ReconcileResult pid bid tid world
  = ReconcileOk !(RollbackState pid bid tid world)
  | ReconcileRollback
      !Tick
      !(HistoryFrame pid bid tid world)
      ![(Tick, [(pid, InputEvent bid)])]
      !(RollbackState pid bid tid world)
  deriving (Eq, Show)

-- | Compare the server's authoritative inputs (and optional quantized snapshot
-- hash) for @tick@ against the local predicted 'HistoryFrame'.
--
-- * On match: advances 'rsConfirmedTick', prunes history frames strictly older
--   than @tick@, and returns 'ReconcileOk'.
-- * On mismatch: rewinds to @tick@, constructs the exact replay schedule from
--   @tick@ through 'rsCurrentTick' (using authoritative inputs at @tick@ and
--   buffered local inputs for subsequent ticks), and returns 'ReconcileRollback'.
confirmServerTick
  :: (Ord pid, Eq bid)
  => Tick
  -> [(pid, InputEvent bid)]
  -> Maybe Word64
  -> RollbackState pid bid tid world
  -> ReconcileResult pid bid tid world
confirmServerTick tick authInputs mServerHash rs =
  case Map.lookup tick (rsHistory rs) of
    Nothing ->
      let rs' = pruneBefore tick (rs {rsConfirmedTick = max (rsConfirmedTick rs) tick})
       in ReconcileOk rs'
    Just frame ->
      let predCanon = sortBy (comparing fst) (hfInputs frame)
          authCanon = sortBy (comparing fst) authInputs
          inputsMatch = predCanon == authCanon
          hashMatch = case mServerHash of
            Nothing -> True
            Just expectedHash -> hashSnapshotInternal (hfSnapshot frame) == expectedHash
       in if inputsMatch && hashMatch
            then
              let rs' = pruneBefore tick (rs {rsConfirmedTick = max (rsConfirmedTick rs) tick})
               in ReconcileOk rs'
            else
              let endTick = max tick (rsCurrentTick rs)
                  futureTicks = if endTick > tick then [tick + 1 .. endTick] else []
                  replaySchedule =
                    (tick, authCanon)
                      : [ ( t
                          , sortBy (comparing fst) (Map.findWithDefault [] t (rsLocalInputs rs))
                          )
                        | t <- futureTicks
                        ]
                  rsPruned = pruneBefore tick rs
                  rsRewound =
                    rsPruned
                      { rsConfirmedTick = max (rsConfirmedTick rs) tick
                      , rsCurrentTick = tick
                      , rsHistory = Map.filterWithKey (\k _ -> k < tick) (rsHistory rsPruned)
                      , rsLocalInputs = Map.filterWithKey (\k _ -> k > tick) (rsLocalInputs rsPruned)
                      }
               in ReconcileRollback tick frame replaySchedule rsRewound

pruneBefore :: Tick -> RollbackState pid bid tid world -> RollbackState pid bid tid world
pruneBefore tick rs =
  rs
    { rsHistory = Map.filterWithKey (\k _ -> k >= tick) (rsHistory rs)
    , rsLocalInputs = Map.filterWithKey (\k _ -> k >= tick) (rsLocalInputs rs)
    , rsPlayedSounds = Set.filter (\(t, _) -> t >= tick) (rsPlayedSounds rs)
    }

-- | Deterministic 64-bit FNV-1a hash over canonically sorted snapshot bodies
-- with 1 mm spatial/velocity quantization and canonical quaternion hemisphere.
hashSnapshot :: Ord bid => Snapshot bid -> Word64
hashSnapshot = hashSnapshotInternal

hashSnapshotInternal :: Snapshot bid -> Word64
hashSnapshotInternal snap =
  let bodies = Map.toAscList (snapBodies snap)
      h0 = fnvMixInt64 fnvOffsetBasis (fromIntegral (length bodies))
   in foldl' (\h (_, bs) -> hashBodyState h bs) h0 bodies

fnvOffsetBasis :: Word64
fnvOffsetBasis = 14695981039346656037

fnvPrime :: Word64
fnvPrime = 1099511628211

fnvMixByte :: Word64 -> Word64 -> Word64
fnvMixByte h byte = (h `xor` (byte .&. 0xFF)) * fnvPrime

fnvMixInt64 :: Word64 -> Int64 -> Word64
fnvMixInt64 h0 n =
  let w = fromIntegral n :: Word64
      h1 = fnvMixByte h0 w
      h2 = fnvMixByte h1 (w `shiftR` 8)
      h3 = fnvMixByte h2 (w `shiftR` 16)
      h4 = fnvMixByte h3 (w `shiftR` 24)
      h5 = fnvMixByte h4 (w `shiftR` 32)
      h6 = fnvMixByte h5 (w `shiftR` 40)
      h7 = fnvMixByte h6 (w `shiftR` 48)
   in fnvMixByte h7 (w `shiftR` 56)

quantizeMm :: Double -> Int64
quantizeMm x
  | isNaN x || isInfinite x = 0
  | otherwise = round (x * 1000.0)

-- | Canonicalize unit quaternion sign so @q@ and @-q@ hash identically.
canonicalQuat :: Quat -> Quat
canonicalQuat q@(Quat x y z w)
  | w < 0 = Quat (-x) (-y) (-z) (-w)
  | w == 0 && x < 0 = Quat (-x) (-y) (-z) (-w)
  | w == 0 && x == 0 && y < 0 = Quat (-x) (-y) (-z) (-w)
  | w == 0 && x == 0 && y == 0 && z < 0 = Quat (-x) (-y) (-z) (-w)
  | otherwise = q

hashVec3 :: Word64 -> Vec3 -> Word64
hashVec3 h (Vec3 x y z) =
  fnvMixInt64 (fnvMixInt64 (fnvMixInt64 h (quantizeMm x)) (quantizeMm y)) (quantizeMm z)

hashQuat :: Word64 -> Quat -> Word64
hashQuat h q =
  let Quat x y z w = canonicalQuat q
   in fnvMixInt64
        (fnvMixInt64 (fnvMixInt64 (fnvMixInt64 h (quantizeMm x)) (quantizeMm y)) (quantizeMm z))
        (quantizeMm w)

hashBodyState :: Word64 -> BodyState -> Word64
hashBodyState h bs =
  let h1 = hashVec3 h (bsPos bs)
      h2 = hashQuat h1 (bsRot bs)
      h3 = hashVec3 h2 (bsVel bs)
   in hashVec3 h3 (bsAngVel bs)

hashSound :: Sound -> Int
hashSound s =
  let raw = case s of
        SoundTone wave f0 f1 dur vol ->
          let wTag :: Int64
              wTag = case wave of
                WaveSine -> 1
                WaveSquare -> 2
                WaveSawtooth -> 3
                WaveTriangle -> 4
              h1 = fnvMixInt64 fnvOffsetBasis wTag
              h2 = fnvMixInt64 h1 (quantizeMm f0)
              h3 = fnvMixInt64 h2 (quantizeMm f1)
              h4 = fnvMixInt64 h3 (quantizeMm dur)
           in fnvMixInt64 h4 (quantizeMm vol)
        SoundNoise dur vol ->
          let h1 = fnvMixInt64 fnvOffsetBasis 99
              h2 = fnvMixInt64 h1 (quantizeMm dur)
           in fnvMixInt64 h2 (quantizeMm vol)
   in fromIntegral raw

-- | Filter out audio commands that were already played at @tick@ during an
-- earlier predicted execution, while preserving all physics commands and any
-- newly triggered sounds.
dedupSounds
  :: Tick
  -> [MultiCommand pid bid]
  -> RollbackState pid bid tid world
  -> ([MultiCommand pid bid], RollbackState pid bid tid world)
dedupSounds tick cmds rs =
  let (revKept, _, played') = foldl' step ([], Map.empty, rsPlayedSounds rs) cmds
   in (reverse revKept, rs {rsPlayedSounds = played'})
  where
    step (acc, seenCounts, played) cmd =
      case extractSound cmd of
        Nothing -> (cmd : acc, seenCounts, played)
        Just sndFx ->
          let baseKey = hashSound sndFx
              occ = Map.findWithDefault (0 :: Int) baseKey seenCounts
              seenCounts' = Map.insert baseKey (occ + 1) seenCounts
              soundKey = (tick, baseKey + occ)
           in if Set.member soundKey played
                then (acc, seenCounts', played)
                else (cmd : acc, seenCounts', Set.insert soundKey played)

    extractSound (WorldCmd (PlaySound s)) = Just s
    extractSound (PlayerCmd _ (PlaySound s)) = Just s
    extractSound _ = Nothing
