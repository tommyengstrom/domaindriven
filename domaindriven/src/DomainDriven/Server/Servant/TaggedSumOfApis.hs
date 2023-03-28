{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module DomainDriven.Server.Servant.TaggedSumOfApis where

import Data.Constraint
import Data.Kind
import DomainDriven.Server.Servant.MangledPathSegment
import GHC.Generics qualified as GHC
import GHC.TypeLits
import Generics.SOP (All, Generic (..), HasDatatypeInfo (..), I, NP (Nil, (:*)), NS, Rep, SListI, SOP)
import Generics.SOP.GGP
import Generics.SOP.Type.Metadata
import Servant
import Servant.OpenApi
import Servant.Server.Generic
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.Router
import Prelude

{-
Todo: examples, equalities etc
Todo: derive SOP.Generic and SOP.HasDatatypeInfo behind the scenes
-}

type Api = Type

type family ToServantApiWithFieldLabelPrefixes t (apis :: [Api]) (infos :: [FieldInfo]) :: Api where
  ToServantApiWithFieldLabelPrefixes t '[api] '[ 'FieldInfo label] = label :> api
  ToServantApiWithFieldLabelPrefixes t (api ': apis) ('FieldInfo label ': infos) =
    (PathSegment label t :> api) :<|> ToServantApiWithFieldLabelPrefixes t apis infos
  ToServantApiWithFieldLabelPrefixes t '[] '[] = TypeError ('Text "Empty api records are not supported!")
  ToServantApiWithFieldLabelPrefixes _ _ _ = TypeError ('Text "What happened here?!")

-- We could define this to be EmptyAPI but that is not supported by
-- ToServantApi and would lead to a (more) confusing type error down the line.

type family GToServantApiWithFieldLabelPrefixes t (apiRecord :: Type) :: Type where
  GToServantApiWithFieldLabelPrefixes t apiRecord = GToServantApiWithFieldLabelPrefixes' t (Code apiRecord) (DatatypeInfoOf apiRecord)

type family GToServantApiWithFieldLabelPrefixes' t (codeForApiRecord :: [[Api]]) (info :: DatatypeInfo) :: Type where
  GToServantApiWithFieldLabelPrefixes' t '[apis] ('ADT _ _ '[ 'Record _ info] _) = ToServantApiWithFieldLabelPrefixes t apis info
  GToServantApiWithFieldLabelPrefixes' t _ info = TypeError ('Text "Servant generics are only supported for record types. " ':<>: 'ShowType info ':<>: 'Text " is not a record.")

data TaggedSumOfApis (tagFromConstructorLabel :: Type) (mkApiRecord :: Type -> Type)

newtype TaggedSumOfServers a = TaggedSumOfServers {unTaggedSumOfServers :: a}
  deriving newtype (GHC.Generic)

instance (GHC.Generic a, All SListI (Code a), GTo a, GFrom a, Rep a ~ SOP I (GCode a)) => Generic (TaggedSumOfServers a)

{-
-- This instance is adapted (and mostly copy-pasted) from HasServer (NamedRoutes mkApiRecord).
instance
  ( HasServer (GToServantApiWithFieldLabelPrefixes t (mkApiRecord AsApi)) context,
    ServerT (GToServantApiWithFieldLabelPrefixes t (mkApiRecord AsApi)) Handler ~ ToServant mkApiRecord (AsServerT Handler),
    forall m. GHC.Generic (mkApiRecord (AsServerT m)),
    forall m. GServer' t mkApiRecord m
  ) =>
  HasServer (TaggedSumOfApis t mkApiRecord) context
  where
  -- We wrap the server record to make the instance less magical.
  type ServerT (TaggedSumOfApis t mkApiRecord) m = TaggedSumOfServers (mkApiRecord (AsServerT m))

  route ::
    Proxy (TaggedSumOfApis t mkApiRecord) ->
    Context context ->
    Delayed env (TaggedSumOfServers (mkApiRecord (AsServerT Handler))) ->
    Router env
  route _ ctx delayed =
    case gServerProof' @t @mkApiRecord @Handler of
      Dict -> route (Proxy @(GToServantApiWithFieldLabelPrefixes t (mkApiRecord AsApi))) ctx (toServant . unTaggedSumOfServers <$> delayed)

  hoistServerWithContext ::
    forall m n.
    Proxy (TaggedSumOfApis t mkApiRecord) ->
    Proxy context ->
    (forall x. m x -> n x) ->
    TaggedSumOfServers (mkApiRecord (AsServerT m)) ->
    TaggedSumOfServers (mkApiRecord (AsServerT n))
  hoistServerWithContext _ pctx nat (TaggedSumOfServers server) = TaggedSumOfServers $ case (gServerProof' @t @mkApiRecord @m, gServerProof' @t @mkApiRecord @n) of
    (Dict, Dict) ->
      fromServant servantSrvN
      where
        servantSrvM :: ServerT (GToServantApiWithFieldLabelPrefixes t (mkApiRecord AsApi)) m =
          toServant server
        servantSrvN :: ServerT (GToServantApiWithFieldLabelPrefixes t (mkApiRecord AsApi)) n =
          hoistServerWithContext (Proxy @(GToServantApiWithFieldLabelPrefixes t (mkApiRecord AsApi))) pctx nat servantSrvM

type GServerConstraints' t mkApiRecord m =
  ( ServerT (GToServantApiWithFieldLabelPrefixes t (mkApiRecord AsApi)) m ~ ToServant mkApiRecord (AsServerT m),
    GServantProduct (GHC.Rep (mkApiRecord (AsServerT m)))
  )

class GServer' (t :: Type) (mkApiRecord :: Type -> Type) (m :: Type -> Type) where
  gServerProof' :: Dict (GServerConstraints' t mkApiRecord m)

-- These equalities don't hold because ToServant gives us nested servers...
instance
  ( ToServant mkApiRecord (AsServerT m) ~ ServerT (ToServantApi mkApiRecord) m,
    GServantProduct (GHC.Rep (mkApiRecord (AsServerT m))),
    ServerT (GToServantApiWithFieldLabelPrefixes t (mkApiRecord AsApi)) m ~ ToServant mkApiRecord (AsServerT m)
  ) =>
  GServer' t mkApiRecord m
  where
  gServerProof' = Dict

instance
  HasOpenApi (GToServantApiWithFieldLabelPrefixes t (mkApiRecord AsApi)) =>
  HasOpenApi (TaggedSumOfApis t mkApiRecord)
  where
  toOpenApi _ = toOpenApi (Proxy :: Proxy (GToServantApiWithFieldLabelPrefixes t (mkApiRecord AsApi)))

-}


-- This instance is adapted (and mostly copy-pasted) from HasServer (NamedRoutes mkApiRecord).
instance
  ( HasServer (GToServantApiWithFieldLabelPrefixes t (mkApiRecord AsApi)) context,
    ServerT (GToServantApiWithFieldLabelPrefixes t (mkApiRecord AsApi)) Handler ~ ToServant mkApiRecord (AsServerT Handler),
    forall m. GHC.Generic (mkApiRecord (AsServerT m)),
    forall m. GServer' t mkApiRecord m
  ) =>
  HasServer (TaggedSumOfApis t mkApiRecord) context
  where
  -- We wrap the server record to make the instance less magical.
  type ServerT (TaggedSumOfApis t mkApiRecord) m = TaggedSumOfServers (mkApiRecord (AsServerT m))

  route ::
    Proxy (TaggedSumOfApis t mkApiRecord) ->
    Context context ->
    Delayed env (TaggedSumOfServers (mkApiRecord (AsServerT Handler))) ->
    Router env
  route _ ctx delayed =
    case gServerProof' @t @mkApiRecord @Handler of
      Dict -> route (Proxy @(GToServantApiWithFieldLabelPrefixes t (mkApiRecord AsApi))) ctx (toServant . unTaggedSumOfServers <$> delayed)

  hoistServerWithContext ::
    forall m n.
    Proxy (TaggedSumOfApis t mkApiRecord) ->
    Proxy context ->
    (forall x. m x -> n x) ->
    TaggedSumOfServers (mkApiRecord (AsServerT m)) ->
    TaggedSumOfServers (mkApiRecord (AsServerT n))
  hoistServerWithContext _ pctx nat (TaggedSumOfServers server) = TaggedSumOfServers $ case (gServerProof' @t @mkApiRecord @m, gServerProof' @t @mkApiRecord @n) of
    (Dict, Dict) ->
      fromServant servantSrvN
      where
        servantSrvM :: ServerT (GToServantApiWithFieldLabelPrefixes t (mkApiRecord AsApi)) m =
          toServant server
        servantSrvN :: ServerT (GToServantApiWithFieldLabelPrefixes t (mkApiRecord AsApi)) n =
          hoistServerWithContext (Proxy @(GToServantApiWithFieldLabelPrefixes t (mkApiRecord AsApi))) pctx nat servantSrvM

type GServerConstraints' t mkApiRecord m =
  ( ServerT (GToServantApiWithFieldLabelPrefixes t (mkApiRecord AsApi)) m ~ ToServant mkApiRecord (AsServerT m),
    GServantProduct (GHC.Rep (mkApiRecord (AsServerT m)))
  )

class GServer' (t :: Type) (mkApiRecord :: Type -> Type) (m :: Type -> Type) where
  gServerProof' :: Dict (GServerConstraints' t mkApiRecord m)

-- These equalities don't hold because ToServant gives us nested servers...
instance
  ( ToServant mkApiRecord (AsServerT m) ~ ServerT (ToServantApi mkApiRecord) m,
    GServantProduct (GHC.Rep (mkApiRecord (AsServerT m))),
    ServerT (GToServantApiWithFieldLabelPrefixes t (mkApiRecord AsApi)) m ~ ToServant mkApiRecord (AsServerT m)
  ) =>
  GServer' t mkApiRecord m
  where
  gServerProof' = Dict

instance
  HasOpenApi (GToServantApiWithFieldLabelPrefixes t (mkApiRecord AsApi)) =>
  HasOpenApi (TaggedSumOfApis t mkApiRecord)
  where
  toOpenApi _ = toOpenApi (Proxy :: Proxy (GToServantApiWithFieldLabelPrefixes t (mkApiRecord AsApi)))

