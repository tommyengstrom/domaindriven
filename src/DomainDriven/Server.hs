{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}

module DomainDriven.Server
    ( mkServer
    , module DomainDriven.Internal.NamedFields
    , HasFieldName(..)
    ) where

import           DomainDriven.Internal.HasFieldName
                                                ( HasFieldName(..) )
import           DomainDriven.Internal.NamedFields
import           DomainDriven.Server.TH
