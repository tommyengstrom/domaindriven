{-# OPTIONS_GHC -Wno-orphans #-}

module Servant.Auth.Internal.ThrowAll.SOP where

import Generics.SOP (I (..), NP (Nil, (:*)), NS (..), SOP (..))
import Servant.Auth.Server.Internal.ThrowAll
import Prelude

instance ThrowAll (NP I '[]) where
    throwAll _ = Nil

instance (ThrowAll (NP I cs), ThrowAll c) => ThrowAll (NP I (c ': cs)) where
    throwAll err = I (throwAll err) :* throwAll err

instance ThrowAll (NP I servers) => ThrowAll (SOP I '[servers]) where
    throwAll = SOP . Z . throwAll
