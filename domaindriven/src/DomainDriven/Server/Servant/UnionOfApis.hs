{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module DomainDriven.Server.Servant.UnionOfApis where

import Data.Kind
import Generics.SOP (NP (Nil, (:*)))
import Servant
import Servant.OpenApi
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.Router
import Prelude

type Api = Type

data UnionOfApis (apis :: [Api]) -- union...

data FlipServerT (m :: Type -> Type) (api :: Api) where
  FlipServerT :: ServerT api m -> FlipServerT m api

type ServerTs' (apis :: [Api]) m = NP (FlipServerT m) apis

class HaveServers (apis :: [Api]) (context :: [Type]) where
  routes :: Context context -> Delayed env (ServerTs' apis Handler) -> Router env
  hoistServersWithContext :: (forall x. m x -> n x) -> ServerTs' apis m -> ServerTs' apis n

instance HaveServers '[] context where
  routes _ _ = StaticRouter mempty mempty
  hoistServersWithContext _ Nil = Nil

instance (HasServer api context, HaveServers apis context) => HaveServers (api ': apis) context where
  routes context delayedServers =
    choice
      (route (Proxy @api) context $ (\(FlipServerT server :* _) -> server) <$> delayedServers)
      (routes @apis @context context $ (\(_ :* servers) -> servers) <$> delayedServers)
  hoistServersWithContext nt (FlipServerT server :* servers) =
    FlipServerT (hoistServerWithContext (Proxy @api) (Proxy @context) nt server)
      :* hoistServersWithContext @apis @context nt servers

newtype ServerTs (apis :: [Api]) m = ServerTs (NP (FlipServerT m) apis)

instance HaveServers apis context => HasServer (UnionOfApis apis) context where
  type ServerT (UnionOfApis apis) m = ServerTs apis m
  route _ context delayedServers = routes context $ (\(ServerTs servers) -> servers) <$> delayedServers
  hoistServerWithContext _ _ nt (ServerTs servers) = ServerTs $ hoistServersWithContext @apis @context nt servers

instance HasOpenApi (UnionOfApis '[]) where
  toOpenApi _ = mempty

instance
  (HasOpenApi api, HasOpenApi (UnionOfApis apis)) =>
  HasOpenApi (UnionOfApis (api ': apis))
  where
  toOpenApi _ = toOpenApi (Proxy @api) <> toOpenApi (Proxy @(UnionOfApis apis))
