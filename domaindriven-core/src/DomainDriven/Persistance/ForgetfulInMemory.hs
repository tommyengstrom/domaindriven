{-# LANGUAGE InstanceSigs #-}

module DomainDriven.Persistance.ForgetfulInMemory where

import Control.DeepSeq (NFData)
import Data.Foldable (toList)
import Data.Generics.Labels ()
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
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
    busy <- newTVarIO HS.empty
    pure $ ForgetfulInMemory state appEvent m0 busy (\_ _ _ -> pure ())

-- | In-memory state with per-index command serialization.
data ForgetfulInMemory model index event = ForgetfulInMemory
    { stateRef :: IORef (HashMap index (model, Seq (Stored event)))
    , apply :: model -> Stored event -> model
    , seed :: model
    , busyIndices :: TVar (HashSet index)
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
    getModel ff index = maybe (seed ff) fst . HM.lookup index <$> readIORef (stateRef ff)
    getEventList ff index = maybe [] (toList . snd) . HM.lookup index <$> readIORef (stateRef ff)
    getEventStream ff index =
        Stream.bracketIO
            (getEventList ff index)
            (const (pure ()))
            Stream.fromList

instance (Hashable index, NFData event) => WriteModel (ForgetfulInMemory model index event) where
    postUpdateHook p index model events = liftIO $ updateHook p index model events
    transactionalUpdate ff index evalCmd = withIndexLock ff index $ do
        model <- getModel ff index
        (returnFun, evs) <- evalCmd model
        storedEvs <- traverse toStored evs
        let newModel = foldl' (apply ff) model storedEvs
            appendHistory :: (model, Seq (Stored event)) -> (model, Seq (Stored event)) -> (model, Seq (Stored event))
            appendHistory (_, new) (_, old) = (newModel, old <> new)
        atomicModifyIORef' (stateRef ff) $ \states ->
            (HM.insertWith appendHistory index (newModel, Seq.fromList storedEvs) states, ())
        pure (newModel, storedEvs, returnFun)

withIndexLock
    :: (MonadUnliftIO m, Hashable index)
    => ForgetfulInMemory model index event
    -> index
    -> m a
    -> m a
withIndexLock ff index = bracket_ (atomically acquire) (atomically release)
  where
    acquire :: STM ()
    acquire = do
        busy <- readTVar (busyIndices ff)
        checkSTM (not (HS.member index busy))
        writeTVar (busyIndices ff) (HS.insert index busy)

    release :: STM ()
    release = modifyTVar' (busyIndices ff) (HS.delete index)
