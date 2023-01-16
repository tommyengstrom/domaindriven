{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}

module DomainDriven.Server
    ( mkServer
    , module DomainDriven.Server.Config
    )
where

import DomainDriven.Server.Config
import DomainDriven.Server.TH
