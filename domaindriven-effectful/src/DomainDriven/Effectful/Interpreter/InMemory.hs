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

-- | Run the Projection' effect using an in-memory backend (new domain API)
runProjectionInMemory
    :: forall domain es a
     . ( Hashable (DomainIndex domain), IOE :> es)
    => ForgetfulInMemory (DomainModel domain) (DomainIndex domain) (DomainEvent domain)
    -> DomainIndex domain
    -> Eff (Projection domain : es) a
    -> Eff es a
runProjectionInMemory backend idx = interpret $ \_ -> \case
    GetModel  -> liftIO $ P.getModel backend idx
    GetEventList  -> liftIO $ P.getEventList backend idx

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
    RunTransaction  idx cmd -> do
        localSeqUnliftIO env $ \unlift -> do
            (model', _, returnFun) <- liftIO $ P.transactionalUpdate backend idx $ \m ->
                unlift (cmd m)
            pure $ returnFun model'
