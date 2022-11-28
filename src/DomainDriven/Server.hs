{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}

module DomainDriven.Server
    ( mkServer
    , module DomainDriven.Internal.NamedFields
    , HasFieldName (..)
    )
where

import DomainDriven.Internal.HasFieldName
    ( HasFieldName (..)
    )
import DomainDriven.Internal.NamedFields
import DomainDriven.Server.TH
