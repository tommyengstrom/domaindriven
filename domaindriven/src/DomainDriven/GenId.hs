{-# LANGUAGE TemplateHaskell #-}

module DomainDriven.GenId where

import Data.UUID (UUID)
import Effectful
import Effectful.TH

-- | Generate identifiers for application entities without requiring application
-- code to depend directly on 'IOE'.
data GenId :: Effect where
    GenId :: GenId m UUID

type instance DispatchOf GenId = 'Dynamic

$(makeEffect ''GenId)
