{-# LANGUAGE InstanceSigs #-}

module DomainDriven.Persistance.ForgetfulInMemory where

import DomainDriven.Persistance.Class
import GHC.Generics (Generic)
import Streamly.Data.Stream.Prelude qualified as Stream
import UnliftIO
import Data.Maybe (fromMaybe)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.Generics.Labels ()
import Data.HashMap.Strict qualified as HM
import Prelude

createForgetful
    :: forall index model event m. MonadIO m
    => (model -> Stored event -> model)
    -> model
    -- ^ initial model
    -> m (ForgetfulInMemory model index event)
createForgetful appEvent m0 = do
    state <- newIORef HM.empty
    evs <- newIORef HM.empty
    lock <- newQSem 1
    pure $ ForgetfulInMemory state appEvent m0 evs lock (\_ _ _ -> pure ())

-- | STM state without event persistance
data ForgetfulInMemory model index event = ForgetfulInMemory
    { stateRef :: IORef (HashMap index model)
    , apply :: model -> Stored event -> model
    , seed :: model
    , events :: IORef (HashMap index [Stored event])
    , lock :: QSem
    , updateHook :: index -> model -> [Stored event] -> IO ()
    }
    deriving (Generic)

instance Hashable index => ReadModel (ForgetfulInMemory model index event) where
    type Model (ForgetfulInMemory model index event) = model
    type Event (ForgetfulInMemory model index event) = event
    type Index (ForgetfulInMemory model index event) = index
    applyEvent = apply
    getModel :: ForgetfulInMemory model index event
             -> index
             -> IO model
    getModel ff index = HM.lookupDefault (seed ff) index <$> readIORef (stateRef ff)
    getEventList ff index = HM.lookupDefault [] index <$> readIORef (events ff)
    getEventStream ff index =
        Stream.bracketIO
            (getEventList ff index)
            (const (pure ()))
            Stream.fromList

instance Hashable index => WriteModel (ForgetfulInMemory model index event) where
    postUpdateHook p index model events = liftIO $ updateHook p index model events
    transactionalUpdate ff index evalCmd =
        bracket_ (waitQSem $ lock ff) (signalQSem $ lock ff) $ do
            model <- HM.lookupDefault (seed ff) index <$> readIORef (stateRef ff)
            (returnFun, evs) <- evalCmd model
            storedEvs <- traverse toStored evs
            let newModel = foldl' (apply ff) model storedEvs
            modifyIORef (events ff)
                $ HM.alter (Just . (<> storedEvs) . fromMaybe []) index
            modifyIORef (stateRef ff) $ HM.insert index newModel
            pure (newModel, storedEvs, returnFun)
