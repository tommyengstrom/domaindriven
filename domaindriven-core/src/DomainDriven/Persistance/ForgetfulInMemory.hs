{-# LANGUAGE InstanceSigs #-}

module DomainDriven.Persistance.ForgetfulInMemory where

import Data.List (foldl')
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
    -> m (ForgetfulInMemory model event)
createForgetful appEvent m0 = do
    state <- newIORef m0
    evs <- newIORef []
    lock <- newQSem 1
    pure $ ForgetfulInMemory state appEvent m0 evs lock (\_ _ -> pure ())

-- | STM state without event persistance
data ForgetfulInMemory model event = ForgetfulInMemory
    { stateRef :: IORef model
    , apply :: model -> Stored event -> model
    , seed :: model
    , events :: IORef [Stored event]
    , lock :: QSem
    , updateHook :: model -> [Stored event] -> IO ()
    }
    deriving (Generic)

instance ReadModel (ForgetfulInMemory model e) where
    type Model (ForgetfulInMemory model e) = model
    type Event (ForgetfulInMemory model e) = e
    applyEvent = apply
    getModel :: ForgetfulInMemory model e -> IO (Model (ForgetfulInMemory model e))
    getModel ff = readIORef $ stateRef ff
    getEventList ff = readIORef $ events ff
    getEventStream ff =
        Stream.bracketIO
            (getEventList ff)
            (const (pure ()))
            Stream.fromList

instance WriteModel (ForgetfulInMemory model e) where
    postUpdateHook p model events = liftIO $ updateHook p model events
    transactionalUpdate ff evalCmd =
        bracket_ (waitQSem $ lock ff) (signalQSem $ lock ff) $ do
            model <- readIORef $ stateRef ff
            (returnFun, evs) <- evalCmd model
            storedEvs <- traverse toStored evs
            let newModel = foldl' (apply ff) model storedEvs
            modifyIORef (events ff) (<> storedEvs)
            writeIORef (stateRef ff) newModel
            pure (newModel, storedEvs, returnFun)
