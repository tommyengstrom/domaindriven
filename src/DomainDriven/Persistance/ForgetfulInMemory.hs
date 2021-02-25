module DomainDriven.Persistance.ForgetfulInMemory where

import           DomainDriven.Internal.Class
import           Prelude
import           GHC.Generics                   ( Generic )
import           Control.Concurrent
import           Data.IORef
import           Control.Monad.Catch
import           Data.List                      ( foldl' )


createForgetful
    :: (model -> Stored event -> model)
    -> model -- ^ initial model
    -> IO (ForgetfulInMemory model event)
createForgetful appEvent m0 = do
    state <- newIORef m0
    evs   <- newIORef []
    lock  <- newQSem 1
    pure $ ForgetfulInMemory state appEvent m0 evs lock

-- | STM state without event persistance
data ForgetfulInMemory model event = ForgetfulInMemory
    { stateRef :: IORef model
    , app      :: model -> Stored event -> model
    , seed     :: model
    , events   :: IORef [Stored event]
    , lock     :: QSem
    }
    deriving Generic

instance ReadModel (ForgetfulInMemory m e) where
    type Model (ForgetfulInMemory m e) = m
    type Event (ForgetfulInMemory m e) = e
    applyEvent ff = app ff
    getModel ff = readIORef $ stateRef ff
    getEvents ff = readIORef $ events ff

instance WriteModel (ForgetfulInMemory m e) where
    transactionalUpdate ff evalCmd =
        bracket_ (waitQSem $ lock ff) (signalQSem $ lock ff) $ do
            model     <- readIORef $ stateRef ff
            (r, evs)  <- evalCmd
            storedEvs <- traverse toStored evs
            let newModel = foldl' (app ff) model storedEvs
            modifyIORef (events ff) (<> storedEvs)
            writeIORef (stateRef ff) newModel
            pure r
