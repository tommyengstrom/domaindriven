{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}

module DomainDriven.Server
    ( mkServer
    , module DomainDriven.Server.Config
    , module P
    )
where

import DomainDriven.Server.Config
import qualified DomainDriven.Server.Param as P
import DomainDriven.Server.TH
