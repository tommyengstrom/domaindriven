{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module DomainDriven.Aggregate where

import Data.Kind (Type)
import DomainDriven.Domain
import DomainDriven.Persistance.Class (NoIndex (..))
import Effectful
import Effectful.Dispatch.Dynamic (send)
import GHC.Stack (HasCallStack)
import Prelude ((.))

-- | The new Aggregate effect with a single domain parameter
data Aggregate (domain :: Type) :: Effect where
    RunTransactionI
        :: DomainIndex domain
        -> ( DomainModel domain
             -> Eff
                    es
                    ( DomainModel domain -> a
                    , [DomainEvent domain]
                    )
           )
        -> Aggregate domain (Eff es) a

type instance DispatchOf (Aggregate domain) = 'Dynamic

runTransactionI
    :: forall domain es a model event index
     . ( HasCallStack
       , Aggregate domain :> es
       , IsDomain domain model event index
       )
    => index
    -> (model -> Eff es (model -> a, [event]))
    -> Eff es a
runTransactionI index = send . RunTransactionI @domain index

runTransaction
    :: forall domain es a model event
     . ( HasCallStack
       , Aggregate domain :> es
       , IsDomain domain model event NoIndex
       )
    => (model -> Eff es (model -> a, [event]))
    -> Eff es a
runTransaction = runTransactionI @domain NoIndex
