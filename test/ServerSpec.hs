{-# LANGUAGE TemplateHaskell #-}

module ServerSpec where

import           Language.Haskell.TH
import           DomainDriven.Server
import           DomainDriven
import           Prelude
import           Servant



$(mkApiDec ''StoreCmd)
