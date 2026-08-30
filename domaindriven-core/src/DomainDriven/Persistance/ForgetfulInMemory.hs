{-# LANGUAGE InstanceSigs #-}

module DomainDriven.Persistance.ForgetfulInMemory where

import Control.DeepSeq (NFData)
import Data.Foldable (toList)
import Data.Generics.Labels ()
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import DomainDriven.Persistance.Class
import GHC.Generics (Generic)
import Streamly.Data.Stream.Prelude qualified as Stream
import UnliftIO
import Prelude

createForgetful
    :: forall index model event m
     . MonadIO m
    => (model -> Stored event -> model)
    -> model
    -- ^ initial model
    -> m (ForgetfulInMemory model index event)
createForgetful appEvent m0 = do
    state <- newIORef HM.empty
    evs <- newIORef HM.empty
    locks <- newMVar HM.empty
    pure $ ForgetfulInMemory state appEvent m0 evs locks (\_ _ _ -> pure ())

-- | In-memory state without event persistance. Commands on the same index are
-- serialized, mirroring the per-index locking of the Postgres backend.
data ForgetfulInMemory model index event = ForgetfulInMemory
    { stateRef :: IORef (HashMap index model)
    , apply :: model -> Stored event -> model
    , seed :: model
    , events :: IORef (HashMap index (Seq (Stored event)))
    , indexLocks :: MVar (HashMap index (MVar ()))
    , updateHook :: index -> model -> [Stored event] -> IO ()
    }
    deriving (Generic)

instance (Hashable index, NFData event) => ReadModel (ForgetfulInMemory model index event) where
    type Model (ForgetfulInMemory model index event) = model
    type Event (ForgetfulInMemory model index event) = event
    type Index (ForgetfulInMemory model index event) = index
    applyEvent = apply
    getModel
        :: MonadIO m
        => ForgetfulInMemory model index event
        -> index
        -> m model
    getModel ff index = HM.lookupDefault (seed ff) index <$> readIORef (stateRef ff)
    getEventList ff index = maybe [] toList . HM.lookup index <$> readIORef (events ff)
    getEventStream ff index =
        Stream.bracketIO
            (getEventList ff index)
            (const (pure ()))
            Stream.fromList

instance (Hashable index, NFData event) => WriteModel (ForgetfulInMemory model index event) where
    postUpdateHook p index model events = liftIO $ updateHook p index model events
    transactionalUpdate ff index evalCmd = withIndexLock ff index $ do
        model <- HM.lookupDefault (seed ff) index <$> readIORef (stateRef ff)
        (returnFun, evs) <- evalCmd model
        storedEvs <- traverse toStored evs
        let newModel = foldl' (apply ff) model storedEvs
        modifyIORef' (events ff) $ HM.insertWith (flip (<>)) index (Seq.fromList storedEvs)
        modifyIORef' (stateRef ff) $ HM.insert index newModel
        pure (newModel, storedEvs, returnFun)

withIndexLock
    :: (MonadUnliftIO m, Hashable index)
    => ForgetfulInMemory model index event
    -> index
    -> m a
    -> m a
withIndexLock ff index action = do
    lock <- modifyMVar (indexLocks ff) $ \locks -> case HM.lookup index locks of
        Just lock -> pure (locks, lock)
        Nothing -> do
            lock <- newMVar ()
            pure (HM.insert index lock locks, lock)
    withMVar lock (const action)
