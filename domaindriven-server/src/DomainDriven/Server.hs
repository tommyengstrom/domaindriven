{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}

module DomainDriven.Server
    ( mkServer
    , module X
    )
where

import DomainDriven.Server.Config as X
import qualified DomainDriven.Server.Param as X
import DomainDriven.Server.TH
import qualified DomainDriven.Server.Types as X
