{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

module DomainDriven.Server.Servant.TaggedSumOfApis where

import Data.Constraint
import Data.Kind
import GHC.Generics qualified as GHC
import GHC.TypeLits
import Generics.SOP (Generic (..), HasDatatypeInfo (..))
import Generics.SOP.Type.Metadata
import Servant
import Servant.Server.Generic
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.Router
import Prelude

{-
Todo: examples, equalities etc
-}

type Api = Type

type family ToServantApiWithFieldLabelPrefixes (apis :: [Api]) (infos :: [FieldInfo]) :: Api where
  ToServantApiWithFieldLabelPrefixes '[api] '[ 'FieldInfo label] = label :> api
  ToServantApiWithFieldLabelPrefixes (api ': apis) ('FieldInfo label ': infos) =
    (label :> api) :<|> ToServantApiWithFieldLabelPrefixes apis infos
  ToServantApiWithFieldLabelPrefixes '[] '[] = TypeError ('Text "Empty api records are not supported!")

-- We could define this to be EmptyAPI but that is not supported by
-- ToServantApi and would lead to a (more) confusing type error down the line.

type family GToServantApiWithFieldLabelPrefixes (apiRecord :: Type) :: Type where
  GToServantApiWithFieldLabelPrefixes apiRecord = GToServantApiWithFieldLabelPrefixes' (Code apiRecord) (DatatypeInfoOf apiRecord)

type family GToServantApiWithFieldLabelPrefixes' (codeForApiRecord :: [[Api]]) (info :: DatatypeInfo) :: Type where
  GToServantApiWithFieldLabelPrefixes' '[apis] ('ADT _ _ '[ 'Record _ info] _) = ToServantApiWithFieldLabelPrefixes apis info
  GToServantApiWithFieldLabelPrefixes' _ info = TypeError ('Text "Servant generics are only supported for record types. " ':<>: 'ShowType info ':<>: 'Text " is not a record.")

data TaggedSumOfApis (mkApiRecord :: Type -> Type)

-- This instance is adapted (and mostly copy-pasted) from HasServer (NamedRoutes mkApiRecord).
instance
  ( HasServer (GToServantApiWithFieldLabelPrefixes (mkApiRecord AsApi)) context,
    ServerT (GToServantApiWithFieldLabelPrefixes (mkApiRecord AsApi)) Handler ~ ToServant mkApiRecord (AsServerT Handler),
    forall m. GHC.Generic (mkApiRecord (AsServerT m)),
    forall m. GServer' mkApiRecord m
  ) =>
  HasServer (TaggedSumOfApis mkApiRecord) context
  where
  type ServerT (TaggedSumOfApis mkApiRecord) m = mkApiRecord (AsServerT m)

  route ::
    Proxy (TaggedSumOfApis mkApiRecord) ->
    Context context ->
    Delayed env (mkApiRecord (AsServerT Handler)) ->
    Router env
  route _ ctx delayed =
    case gServerProof' @mkApiRecord @Handler of
      Dict -> route (Proxy @(GToServantApiWithFieldLabelPrefixes (mkApiRecord AsApi))) ctx (toServant <$> delayed)

  hoistServerWithContext ::
    forall m n.
    Proxy (TaggedSumOfApis mkApiRecord) ->
    Proxy context ->
    (forall x. m x -> n x) ->
    mkApiRecord (AsServerT m) ->
    mkApiRecord (AsServerT n)
  hoistServerWithContext _ pctx nat server =
    case (gServerProof' @mkApiRecord @m, gServerProof' @mkApiRecord @n) of
      (Dict, Dict) ->
        fromServant servantSrvN
        where
          servantSrvM :: ServerT (ToServantApi mkApiRecord) m =
            toServant server
          servantSrvN :: ServerT (ToServantApi mkApiRecord) n =
            hoistServerWithContext (Proxy @(GToServantApiWithFieldLabelPrefixes (mkApiRecord AsApi))) pctx nat servantSrvM

type GServerConstraints' mkApiRecord m =
  ( ToServant mkApiRecord (AsServerT m) ~ ServerT (ToServantApi mkApiRecord) m,
    ServerT (GToServantApiWithFieldLabelPrefixes (mkApiRecord AsApi)) m ~ ToServant mkApiRecord (AsServerT m),
    GServantProduct (GHC.Rep (mkApiRecord (AsServerT m)))
  )

class GServer' (mkApiRecord :: Type -> Type) (m :: Type -> Type) where
  gServerProof' :: Dict (GServerConstraints' mkApiRecord m)

instance
  ( ToServant mkApiRecord (AsServerT m) ~ ServerT (ToServantApi mkApiRecord) m,
    GServantProduct (GHC.Rep (mkApiRecord (AsServerT m))),
    ServerT (GToServantApiWithFieldLabelPrefixes (mkApiRecord AsApi)) m ~ ToServant mkApiRecord (AsServerT m)
  ) =>
  GServer' mkApiRecord m
  where
  gServerProof' = Dict
