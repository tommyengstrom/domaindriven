{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module DomainDriven.Effectful.Interpreter.InMemory where

import DomainDriven.Effectful.Aggregate
import DomainDriven.Effectful.Projection
import DomainDriven.Effectful.Domain
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

-- | Run the Projection' effect using an in-memory backend (new domain API)
runProjectionInMemoryD
    :: forall domain es a
     . ( Hashable (DomainIndex domain), IOE :> es)
    => ForgetfulInMemory (DomainModel domain) (DomainIndex domain) (DomainEvent domain)
    -> DomainIndex domain
    -> Eff (Projection' domain : es) a
    -> Eff es a
runProjectionInMemoryD backend idx = interpret $ \_ -> \case
    GetModel' _ -> liftIO $ P.getModel backend idx
    GetEventList' _ -> liftIO $ P.getEventList backend idx

-- | Run the Aggregate' effect using an in-memory backend (new domain API)
runAggregateInMemoryD
    :: forall domain es a
     . ( IOE :> es
       , WriteModel (ForgetfulInMemory (DomainModel domain) (DomainIndex domain) (DomainEvent domain))
       )
    => ForgetfulInMemory (DomainModel domain) (DomainIndex domain) (DomainEvent domain)
    -> Eff (Aggregate' domain : es) a
    -> Eff es a
runAggregateInMemoryD backend = interpret $ \env -> \case
    RunTransaction' _ idx cmd -> do
        localSeqUnliftIO env $ \unlift -> do
            (model', _, returnFun) <- liftIO $ P.transactionalUpdate backend idx $ \m -> 
                unlift (cmd m)
            pure $ returnFun model'