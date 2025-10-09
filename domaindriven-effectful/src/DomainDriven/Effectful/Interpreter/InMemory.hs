{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module DomainDriven.Effectful.Interpreter.InMemory
    ( module DomainDriven.Effectful.Interpreter.InMemory
    , createForgetful
    , ForgetfulInMemory
    ) where

import Data.Hashable (Hashable)
import DomainDriven.Effectful.Aggregate
import DomainDriven.Effectful.Domain
import DomainDriven.Effectful.Projection
import DomainDriven.Persistance.Class (WriteModel)
import DomainDriven.Persistance.Class qualified as P
import DomainDriven.Persistance.ForgetfulInMemory
import Effectful
import Effectful.Dispatch.Dynamic
import Prelude

-- | Run the Projection' effect using an in-memory backend (new domain API)
runProjectionInMemory
    :: forall domain es a
     . (Hashable (DomainIndex domain), IOE :> es)
    => ForgetfulInMemory (DomainModel domain) (DomainIndex domain) (DomainEvent domain)
    -> Eff (Projection domain : es) a
    -> Eff es a
runProjectionInMemory backend = interpret $ \_ -> \case
    GetModelI idx -> liftIO $ P.getModel backend idx
    GetEventListI idx -> liftIO $ P.getEventList backend idx

-- | Run the Aggregate effect using an in-memory backend (new domain API)
runAggregateInMemory
    :: forall domain es a
     . ( IOE :> es
       , WriteModel
            (ForgetfulInMemory (DomainModel domain) (DomainIndex domain) (DomainEvent domain))
       )
    => ForgetfulInMemory (DomainModel domain) (DomainIndex domain) (DomainEvent domain)
    -> Eff (Aggregate domain : es) a
    -> Eff es a
runAggregateInMemory backend = interpret $ \env -> \case
    RunTransactionI idx cmd -> do
        localSeqUnlift env $ \unlift -> do
            (model', _, returnFun) <-
                P.transactionalUpdate backend idx $
                    unlift . cmd
            pure $ returnFun model'
