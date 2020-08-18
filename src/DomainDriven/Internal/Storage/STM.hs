{-# LANGUAGE AllowAmbiguousTypes #-}
module DomainDriven.Internal.Storage.STM where

import           DomainDriven.Internal.Class
import           RIO
import           Data.Kind


data StorageResult err a
    = StorageError Text
    | CmdError err
    deriving (Show, Exception)

--class PersistanceHandler a model event | a -> model, a -> event where
--    getModel :: a -> IO model
--    getEvents :: a -> IO [event]
--    transactionalUpdate
--        ::  (model -> event -> model)         -- The apply function of the model
--        -> (model -> Either err (a, [event]))  -- The continuation returned by CmdHandler
--        -> IO (StorageResult err a)

class PersistanceHandler a where
    type Event a :: Type
    type Model a :: Type
    getModel :: a -> IO (Model a)
    getEvents :: a -> IO [(Event a)] -- This should really be a stream of some sort!
    transactionalUpdate
        :: (Model a -> Event a -> Model a)         -- The apply function of the model
        -> (Model a -> Either err (ret, [Event a])) -- The continuation returned by CmdHandler
        -> IO (StorageResult err ret)

data FileAndStm model event = FileAndStm
    { eventChan :: TChan (Stored event)
    , stateTVar :: TVar model
    }
    deriving Generic

instance PersistanceHandler (FileAndStm model event) where
    type Event (FileAndStm model event) = event
    type Model (FileAndStm model event) = model
    getModel (FileAndStm _ tvar) = readTVarIO tvar
    getEvents _ = undefined
    transactionalUpdate = undefined



runCmd
    :: Exception err
    => Domain model event
    -> CmdHandler model event cmd err
    -> cmd a
    -> IO a
runCmd (Domain pm appEvent tvar) cmdRunner cmd = do
    cmdTransaction <- cmdRunner cmd
    atomically $ do
        m         <- readTVar tvar
        (r, evs)  <- either throwM pure $ cmdTransaction m
        storedEvs <- traverse (persistEvent pm) evs
        let newModel = foldl' appEvent m storedEvs
        writeTVar tvar newModel
        pure r
