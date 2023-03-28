{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module DomainDriven.Server.Servant.MangledPathSegment where

import Data.Char qualified as Char
import Data.Kind
import Data.OpenApi (prependPath)
import Data.Text qualified as Text
import GHC.TypeLits
import Servant
import Servant.OpenApi
import Servant.Server.Internal.Router
import Prelude

{-
Todo: examples, equalities etc
-}

data PathSegment (segment :: Symbol) (mangler :: Type)

class MangleSymbol mangler where
  mangleSymbol :: String -> String

instance
  (KnownSymbol path, HasServer api context, MangleSymbol mangler) =>
  HasServer (PathSegment path mangler :> api) context
  where
  type ServerT (PathSegment path mangler :> api) m = ServerT api m

  route Proxy context subserver =
    pathRouter
      (Text.pack $ mangleSymbol @mangler $ symbolVal proxyPath)
      (route (Proxy :: Proxy api) context subserver)
    where
      proxyPath = Proxy :: Proxy path
  hoistServerWithContext _ = hoistServerWithContext (Proxy :: Proxy api)

data Capitalise

instance MangleSymbol Capitalise where
  mangleSymbol (c : cs) = Char.toUpper c : cs
  mangleSymbol [] = []

instance
  (MangleSymbol mangler, KnownSymbol symbol, HasOpenApi api) =>
  HasOpenApi (PathSegment symbol mangler :> api)
  where
  toOpenApi _ = prependPath piece (toOpenApi (Proxy :: Proxy api))
    where
      piece = mangleSymbol @mangler $ symbolVal (Proxy :: Proxy symbol)
