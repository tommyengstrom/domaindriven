{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module DomainDriven.Effectful.Interpreter.InMemory
    ( module DomainDriven.Effectful.Interpreter.InMemory
    ) where

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

-- | Run the Projection' effect using an in-memory backend (new domain API)
runProjectionInMemory
    :: forall domain es a
     . ( Hashable (DomainIndex domain), IOE :> es)
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
       , WriteModel (ForgetfulInMemory (DomainModel domain) (DomainIndex domain) (DomainEvent domain))
       )
    => ForgetfulInMemory (DomainModel domain) (DomainIndex domain) (DomainEvent domain)
    -> Eff (Aggregate domain : es) a
    -> Eff es a
runAggregateInMemory backend = interpret $ \env -> \case
    RunTransactionI idx cmd -> do
        localSeqUnlift env $ \unlift -> do
            (model', _, returnFun) <- P.transactionalUpdate backend idx
                $ unlift cmd
            pure $ returnFun model'
