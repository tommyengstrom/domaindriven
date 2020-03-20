{-# LANGUAGE TemplateHaskell #-}

module ServerSpec where

import           Language.Haskell.TH
import           DomainDriven.Server
import           Prelude
import           Servant

$(mkEndpoints ''StoreCmd)
