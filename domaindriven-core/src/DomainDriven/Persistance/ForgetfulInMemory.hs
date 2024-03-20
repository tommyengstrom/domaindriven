{-# LANGUAGE InstanceSigs #-}

module DomainDriven.Persistance.ForgetfulInMemory where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.List (foldl')
import Data.Maybe
import DomainDriven.Persistance.Class
import GHC.Generics (Generic)
import Streamly.Data.Stream.Prelude qualified as Stream
import UnliftIO
import Prelude

createForgetful
    :: MonadIO m
    => (model -> Stored event -> model)
    -> model
    -- ^ initial model
    -> m (ForgetfulInMemory model index event)
createForgetful appEvent m0 = do
    state <- newIORef HM.empty
    evs <- newIORef HM.empty
    lock <- newQSem 1
    pure $ ForgetfulInMemory state appEvent m0 evs lock

-- | STM state without event persistance
data ForgetfulInMemory model index event = ForgetfulInMemory
    { stateRef :: IORef (HashMap index model)
    , apply :: model -> Stored event -> model
    , seed :: model
    , events :: IORef (HashMap index [Stored event])
    , lock :: QSem
    }
    deriving (Generic)

instance (Eq index, Hashable index) => ReadModel (ForgetfulInMemory model index e) where
    type Model (ForgetfulInMemory model index e) = model
    type Event (ForgetfulInMemory model index e) = e
    type Index (ForgetfulInMemory model index e) = index
    applyEvent = apply
    getModel ff index = do
        models <- readIORef $ stateRef ff
        case HM.lookup index models of
            Just model -> pure model
            Nothing -> pure $ seed ff
    getEventList ff index = fromMaybe [] . HM.lookup index <$> readIORef (events ff)
    getEventStream ff index =
        Stream.bracketIO
            (getEventList ff index)
            (const (pure ()))
            Stream.fromList

instance (Hashable index, Eq index) => WriteModel (ForgetfulInMemory model index e) where
    transactionalUpdate ff index evalCmd =
        bracket_ (waitQSem $ lock ff) (signalQSem $ lock ff) $ do
            model <- liftIO $ getModel ff index
            (returnFun, evs) <- evalCmd model
            storedEvs <- traverse toStored evs
            let newModel = foldl' (apply ff) model storedEvs
            modifyIORef (events ff) (HM.update (\a -> Just $ a <> storedEvs) index)
            modifyIORef (stateRef ff) (HM.insert index newModel)
            pure $ returnFun newModel
