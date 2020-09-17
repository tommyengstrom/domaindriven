module DomainDriven.Persistance.ForgetfulSTM where

import           DomainDriven.Internal.Class
import           RIO
import           GHC.IO.Unsafe                  ( unsafePerformIO )


data PersistanceError = EncodingError String
    deriving (Show, Eq, Typeable, Exception)

-- | STM state without event persistance
data ForgetfulSTM model event = ForgetfulSTM
    { stateTVar :: TVar model
    , app       :: model -> Stored event -> model
    , seed      :: model
    }
    deriving Generic

instance ReadModel (ForgetfulSTM m e) where
    type Model (ForgetfulSTM m e) = m
    type Event (ForgetfulSTM m e) = e
    applyEvent'  ff = app ff
    getModel' ff = readTVarIO $ stateTVar ff
------------------------------------------------------------------------------------------
-- The old stuff
------------------------------------------------------------------------------------------
createForgetfulSTM
    :: (model -> Stored event -> model)
    -> model -- ^ initial model
    -> IO (DomainModel (ForgetfulSTM model event) model event)
createForgetfulSTM appEvent m0 = do
    tvar <- newTVarIO m0
    pure $ DomainModel (ForgetfulSTM tvar) appEvent

instance PersistanceHandler (ForgetfulSTM model event) model event where
    getModel (ForgetfulSTM tvar) = readTVarIO tvar
    getEvents _ = pure [] -- ^ Events are not persisted
    transactionalUpdate (ForgetfulSTM tvar) appEvent evalCmd = do
        atomically $ do
            m         <- readTVar tvar
            (r, evs)  <- either throwM pure $ evalCmd m
            storedEvs <- for evs $ \e -> do
                let s = unsafePerformIO $ toStored e
                pure s
            let newModel = foldl' appEvent m storedEvs
            writeTVar tvar newModel
            pure r
