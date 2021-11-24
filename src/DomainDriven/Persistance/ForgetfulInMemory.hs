module DomainDriven.Persistance.ForgetfulInMemory where

import           Data.List                      ( foldl' )
import           DomainDriven.Internal.Class
import           GHC.Generics                   ( Generic )
import           Prelude
import           UnliftIO


createForgetful
    :: MonadIO m
    => (model -> Stored event -> model)
    -> model -- ^ initial model
    -> m (ForgetfulInMemory model event)
createForgetful appEvent m0 = do
    state <- newIORef m0
    evs   <- newIORef []
    lock  <- newQSem 1
    pure $ ForgetfulInMemory state appEvent m0 evs lock

-- | STM state without event persistance
data ForgetfulInMemory model event = ForgetfulInMemory
    { stateRef :: IORef model
    , apply    :: model -> Stored event -> model
    , seed     :: model
    , events   :: IORef [Stored event]
    , lock     :: QSem
    }
    deriving Generic

instance ReadModel (ForgetfulInMemory model e) where
    type Model (ForgetfulInMemory model e) = model
    type Event (ForgetfulInMemory model e) = e
    applyEvent ff = apply ff
    getModel ff = readIORef $ stateRef ff
    getEvents ff = readIORef $ events ff

instance WriteModel (ForgetfulInMemory model e)  where
    transactionalUpdate ff evalCmd =
        bracket_ (waitQSem $ lock ff) (signalQSem $ lock ff) $ do
            model            <- readIORef $ stateRef ff
            (returnFun, evs) <- evalCmd
            storedEvs        <- traverse toStored evs
            let newModel = foldl' (apply ff) model storedEvs
            modifyIORef (events ff) (<> storedEvs)
            writeIORef (stateRef ff) newModel
            pure $ returnFun newModel
