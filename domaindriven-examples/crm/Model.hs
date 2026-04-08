-- | Map-based model — the current state derived from events.
module Model where

import Data.Map.Strict (Map)
import DomainDriven (Domain)
import DomainDriven.Persistance.Class (NoIndex)
import Event (CrmEvent)
import Types (Customer, CustomerId)
import Prelude

--------------------------------------------------------------------------------
-- Model & Domain
--------------------------------------------------------------------------------

-- | The CRM model is a Map of customers keyed by CustomerId.
-- Using NoIndex because we have a single aggregate.
newtype CrmModel = CrmModel
    { customers :: Map CustomerId Customer
    }
    deriving stock (Show, Eq)

emptyCrmModel :: CrmModel
emptyCrmModel = CrmModel mempty

type CrmDomain = Domain CrmModel CrmEvent NoIndex
