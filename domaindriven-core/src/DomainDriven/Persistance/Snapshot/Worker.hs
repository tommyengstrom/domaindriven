module DomainDriven.Persistance.Snapshot.Worker
    ( SnapshotWriter
    , SnapshotJob (..)
    , SnapshotEnqueueResult (..)
    , newSnapshotWriter
    , enqueueSnapshot
    , closeSnapshotWriter
    , flushSnapshots
    )
where

import Control.Concurrent.MVar (MVar, modifyMVarMasked, modifyMVarMasked_, newMVar)
import Control.Exception (SomeException, finally, mask_, onException)
import Control.Monad (void)
import Data.Foldable (traverse_)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Int (Int64)
import Data.Sequence (Seq, ViewL (..), (|>))
import Data.Sequence qualified as Seq
import Data.Unique (Unique, newUnique)
import DomainDriven.Persistance.Snapshot
import System.Timeout qualified as Timeout
import UnliftIO.Async (Async, asyncWithUnmask, cancel)
import UnliftIO.Exception (tryAny)
import UnliftIO.STM (TVar, atomically, newTVarIO, readTVar, retrySTM, writeTVar)
import Prelude

data SnapshotWriter index = SnapshotWriter
    { writerState :: MVar (WriterState index)
    , writerActivity :: TVar WriterActivity
    , queueCapacity :: Int
    }

data WriterState index = WriterState
    { pendingOrder :: Seq index
    , pendingJobs :: HashMap index SnapshotJob
    , runningWorker :: Maybe (Unique, Async ())
    , writerClosed :: Bool
    }

data WriterActivity = WriterIdle | WriterBusy | WriterClosed

data SnapshotJob = SnapshotJob
    { jobEventNumber :: !Int64
    , jobAction :: IO ()
    , jobFailure :: SomeException -> IO ()
    }

data SnapshotEnqueueResult = SnapshotQueued | SnapshotQueueFull | SnapshotWriterStopped
    deriving (Show, Eq)

newSnapshotWriter :: SnapshotQueueCapacity -> IO (SnapshotWriter index)
newSnapshotWriter capacity = do
    state <- newMVar (WriterState Seq.empty HM.empty Nothing False)
    activity <- newTVarIO WriterIdle
    pure $ SnapshotWriter state activity (maximumPendingSnapshots capacity)

enqueueSnapshot
    :: Hashable index
    => SnapshotWriter index
    -> index
    -> SnapshotJob
    -> IO SnapshotEnqueueResult
enqueueSnapshot writer index job = modifyMVarMasked (writerState writer) $ \state ->
    if writerClosed state
        then pure (state, SnapshotWriterStopped)
        else case HM.lookup index (pendingJobs state) of
            Just previous ->
                pure
                    ( state{pendingJobs = HM.insert index (newestJob previous job) (pendingJobs state)}
                    , SnapshotQueued
                    )
            Nothing
                | HM.size (pendingJobs state) >= queueCapacity writer -> pure (state, SnapshotQueueFull)
                | otherwise -> do
                    running <- case runningWorker state of
                        Just worker -> pure (Just worker)
                        Nothing -> Just <$> startWorker writer
                    pure
                        ( state
                            { pendingOrder = pendingOrder state |> index
                            , pendingJobs = HM.insert index job (pendingJobs state)
                            , runningWorker = running
                            }
                        , SnapshotQueued
                        )
  where
    newestJob :: SnapshotJob -> SnapshotJob -> SnapshotJob
    newestJob previous candidate
        | jobEventNumber previous > jobEventNumber candidate = previous
        | otherwise = candidate

-- The caller holds writerState until the worker handle and queued work are published.
startWorker :: Hashable index => SnapshotWriter index -> IO (Unique, Async ())
startWorker writer = do
    identity <- newUnique
    atomically $ writeTVar (writerActivity writer) WriterBusy
    worker <-
        asyncWithUnmask (\unmask -> unmask (drainQueue writer) `finally` finishWorker writer identity)
            `onException` atomically (writeTVar (writerActivity writer) WriterIdle)
    pure (identity, worker)

drainQueue :: Hashable index => SnapshotWriter index -> IO ()
drainQueue writer = do
    next <- modifyMVarMasked (writerState writer) $ \state ->
        case Seq.viewl (pendingOrder state) of
            EmptyL -> do
                atomically $ writeTVar (writerActivity writer) $
                    if writerClosed state then WriterClosed else WriterIdle
                pure (state{runningWorker = Nothing}, Nothing)
            index :< remaining ->
                pure
                    ( state{pendingOrder = remaining, pendingJobs = HM.delete index (pendingJobs state)}
                    , HM.lookup index (pendingJobs state)
                    )
    case next of
        Nothing -> pure ()
        Just job -> do
            tryAny (jobAction job) >>= \case
                Left failure -> void $ tryAny (jobFailure job failure)
                Right () -> pure ()
            drainQueue writer

finishWorker :: SnapshotWriter index -> Unique -> IO ()
finishWorker writer identity = modifyMVarMasked_ (writerState writer) $ \state ->
    case runningWorker state of
        Just (current, _) | current == identity -> do
            atomically $ writeTVar (writerActivity writer) $
                if writerClosed state then WriterClosed else WriterIdle
            pure state{runningWorker = Nothing, pendingOrder = Seq.empty, pendingJobs = HM.empty}
        Just _ -> pure state
        Nothing -> pure state

closeSnapshotWriter :: SnapshotWriter index -> IO ()
closeSnapshotWriter writer = mask_ $ do
    worker <- modifyMVarMasked (writerState writer) $ \state -> do
        atomically $ writeTVar (writerActivity writer) WriterClosed
        pure
            ( state{writerClosed = True, pendingOrder = Seq.empty, pendingJobs = HM.empty}
            , snd <$> runningWorker state
            )
    traverse_ cancel worker

flushSnapshots :: SnapshotTimeout -> SnapshotWriter index -> IO SnapshotFlushResult
flushSnapshots duration writer = do
    result <- Timeout.timeout (ceiling $ snapshotTimeoutDuration duration * 1000000) $
        atomically $ readTVar (writerActivity writer) >>= \case
            WriterIdle -> pure SnapshotFlushCompleted
            WriterBusy -> retrySTM
            WriterClosed -> pure SnapshotFlushClosed
    pure $ case result of
        Nothing -> SnapshotFlushTimedOut
        Just completed -> completed
