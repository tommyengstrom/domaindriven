module DomainDriven.Persistance.ForgetfulSTM where

import           DomainDriven.Internal.Class
import           RIO
import           GHC.IO.Unsafe                  ( unsafePerformIO )


data ForgetfulError = ForgetfulError Text
    deriving (Show, Eq, Typeable, Exception)

createForgetfulSTM
    :: (model -> Stored event -> model)
    -> model -- ^ initial model
    -> IO (ForgetfulSTM model event)
createForgetfulSTM appEvent m0 = do
    state <- newTVarIO m0
    evs <- newTVarIO []
    pure $ ForgetfulSTM state appEvent m0 evs

-- | STM state without event persistance
data ForgetfulSTM model event = ForgetfulSTM
    { stateTVar :: TVar model
    , app       :: model -> Stored event -> model
    , seed      :: model
    , events    :: TVar [Stored event]
    }
    deriving Generic

instance ReadModel (ForgetfulSTM m e) where
    type Model (ForgetfulSTM m e) = m
    type Event (ForgetfulSTM m e) = e
    applyEvent'  ff = app ff
    getModel' ff = readTVarIO $ stateTVar ff
    getEvents' ff = readTVarIO $ events ff

instance WriteModel (ForgetfulSTM m e) where
    type Error (ForgetfulSTM m e) = ForgetfulError
    transactionalUpdate' ff evalCmd =
        atomically $ do
            let tvar = stateTVar ff
            m         <- readTVar tvar
            (r, evs)  <- either throwM pure $ evalCmd m
            storedEvs <- for evs $ \e -> do
                let s = unsafePerformIO $ toStored e
                pure s
            let newModel = foldl' (app ff) m storedEvs
            modifyTVar (events ff) (<> storedEvs)
            writeTVar tvar newModel

            pure r

