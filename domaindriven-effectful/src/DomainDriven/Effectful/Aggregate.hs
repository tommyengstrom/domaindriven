{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module DomainDriven.Effectful.Aggregate where

import Data.Kind (Type)
import Data.Type.Equality
import DomainDriven.Effectful.Domain
import DomainDriven.Persistance.Class (NoIndex (..))
import Effectful
import Effectful.TH

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

$(makeEffect ''Aggregate)

runTransaction
    :: forall domain es a
     . Aggregate domain :> es
    => DomainIndex domain ~ NoIndex
    => ( DomainModel domain -> Eff
            es
            ( DomainModel domain -> a
            , [DomainEvent domain]
            )
       )
    -> Eff es a
runTransaction = runTransactionI NoIndex
