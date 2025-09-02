{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DomainDriven.Effectful.Interpreter.InMemory where

import DomainDriven.Effectful.Aggregate
import DomainDriven.Effectful.Projection
import DomainDriven.Persistance.Class (WriteModel)
import qualified DomainDriven.Persistance.Class as P
import DomainDriven.Persistance.ForgetfulInMemory
import Effectful
import Effectful.Dispatch.Dynamic
import Data.Hashable (Hashable)
import Prelude

-- | Run the Projection effect using an in-memory backend
runProjectionInMemory
    :: forall model event index es a
     . (Hashable index, IOE :> es)
    => ForgetfulInMemory model index event  -- ^ The in-memory persistence backend
    -> index                                 -- ^ The aggregate index to query
    -> Eff (Projection model event index : es) a
    -> Eff es a
runProjectionInMemory backend idx = interpret $ \_ -> \case
    GetModel -> liftIO $ P.getModel backend idx
    GetEventList -> liftIO $ P.getEventList backend idx

-- | Run the Aggregate effect using an in-memory backend
runAggregateInMemory
    :: forall model event index es a
     . ( IOE :> es
       , WriteModel (ForgetfulInMemory model index event)
       )
    => ForgetfulInMemory model index event  -- ^ The in-memory persistence backend
    -> Eff (Aggregate model event index : es) a
    -> Eff es a
runAggregateInMemory backend = interpret $ \env -> \case
    RunTransaction idx cmd -> do
        -- We need to run the effectful command with proper unlift
        localSeqUnliftIO env $ \unlift -> do
            (model', _, returnFun) <- liftIO $ P.transactionalUpdate backend idx $ \m -> 
                -- Convert Eff to IO using unlift
                unlift (cmd m)
            -- Apply the return function to get the final result
            pure $ returnFun model'