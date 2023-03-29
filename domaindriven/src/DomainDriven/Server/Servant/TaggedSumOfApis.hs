{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module DomainDriven.Server.Servant.TaggedSumOfApis where

import Data.Constraint
import Data.Kind
import DomainDriven.Server.Servant.MangledPathSegment
import DomainDriven.Server.Servant.UnionOfApis (UnionOfApis)
import GHC.Generics qualified as GHC
import GHC.TypeLits
import Generics.SOP (All, Generic (..), HasDatatypeInfo (..), I (..), NP (Nil, (:*)), NS (..), Rep, SListI, SOP (..), unSOP)
import Generics.SOP.GGP
import Generics.SOP.Type.Metadata
import Servant
import Servant.Auth.Server.Internal.ThrowAll
import Servant.OpenApi
import Servant.Server.Generic
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.Router
import Prelude

-- move this somewhere?  Servant.Auth.Server.Internal.ThrowAll.SOP ?
instance ThrowAll (NP I '[]) where
  throwAll _ = Nil

instance (ThrowAll (NP I cs), ThrowAll c) => ThrowAll (NP I (c ': cs)) where
  throwAll err = I (throwAll err) :* throwAll err

instance ThrowAll (NP I servers) => ThrowAll (SOP I '[servers]) where
  throwAll = SOP . Z . throwAll

instance
  ( Generic (TaggedSumOfServers a),
    ThrowAll (SOP I (Code (TaggedSumOfServers a)))
  ) =>
  ThrowAll (TaggedSumOfServers a)
  where
  throwAll = to . throwAll @(SOP I (Code (TaggedSumOfServers a)))

type Api = Type

{-
Todo: examples, equalities etc
Todo: derive SOP.Generic and SOP.HasDatatypeInfo behind the scenes
-}

-- attach 'tagfromconstructorlabel' to mkApiRecord?
data TaggedSumOfApis (tagFromConstructorLabel :: Type) (mkApiRecord :: Type -> Type)

-- recordOfServers?
newtype TaggedSumOfServers a = TaggedSumOfServers {unTaggedSumOfServers :: a}
  deriving newtype (GHC.Generic)

instance (GHC.Generic a, All SListI (Code a), GTo a, GFrom a, Rep a ~ SOP I (GCode a)) => Generic (TaggedSumOfServers a)

-- move to GenericRecord?
type family GenericRecordFields (record :: Type) :: [Type] where
  GenericRecordFields record = GenericRecordFields' (Code record)

type family GenericRecordFields' (code :: [[Type]]) :: [Type] where
  GenericRecordFields' '[fields] = fields
  GenericRecordFields' _ = TypeError ('Text "not a record!")

type family GenericRecordFieldInfos (record :: Type) :: [FieldInfo] where
  GenericRecordFieldInfos record = GenericRecordFieldInfos' (DatatypeInfoOf record)

type family GenericRecordFieldInfos' (info :: DatatypeInfo) :: [FieldInfo] where
  GenericRecordFieldInfos' ('ADT _ _ '[ 'Record _ infos] _) = infos
  GenericRecordFieldInfos' _ = TypeError ('Text "not a record!")

class GenericRecord (record :: Type) where
  recordFromFields :: NP I (GenericRecordFields record) -> record
  recordToFields :: record -> NP I (GenericRecordFields record)

class TaggedSumOfServersFields (mkApiRecord :: Type -> Type) (m :: Type -> Type) where
  taggedSumOfServersFromFields :: NP I (ServerTs (GenericRecordFields (mkApiRecord AsApi)) m) -> mkApiRecord (AsServerT m)
  taggedSumOfServersToFields :: mkApiRecord (AsServerT m) -> NP I (ServerTs (GenericRecordFields (mkApiRecord AsApi)) m)

instance
  ( Generic (mkApiRecord (AsServerT m)),
    Code (mkApiRecord (AsServerT m)) ~ '[ServerTs (GenericRecordFields (mkApiRecord AsApi)) m]
  ) =>
  TaggedSumOfServersFields mkApiRecord m
  where
  taggedSumOfServersFromFields = to @(mkApiRecord (AsServerT m)) . SOP . Z
  taggedSumOfServersToFields x = case unSOP $ from x of
    Z servers -> servers
    S y -> case y of {}

class TaggedSumOfApisHasServers t (apis :: [Api]) (infos :: [FieldInfo]) (context :: [Type]) where
  type ServerTs apis (m :: Type -> Type) :: [Type]
  type TaggedApis t apis infos :: [Api]
  taggedSumOfRoutes :: Context context -> Delayed env (NP I (ServerTs apis Handler)) -> Router env
  hoistTaggedServersWithContext :: (forall x. m x -> n x) -> NP I (ServerTs apis m) -> NP I (ServerTs apis n)

instance TaggedSumOfApisHasServers t '[] '[] context where
  type ServerTs '[] m = '[]
  type TaggedApis t '[] '[] = '[]
  taggedSumOfRoutes _ _ = StaticRouter mempty mempty
  hoistTaggedServersWithContext _ Nil = Nil

instance
  (HasServer (PathSegment label t :> api) context, TaggedSumOfApisHasServers t apis infos context) =>
  TaggedSumOfApisHasServers t (api ': apis) ('FieldInfo label ': infos) context
  where
  type ServerTs (api ': apis) m = ServerT api m ': ServerTs apis m
  type TaggedApis t (api ': apis) ('FieldInfo label ': infos) = (PathSegment label t :> api) ': TaggedApis t apis infos

  taggedSumOfRoutes context delayedServers =
    choice
      (route (Proxy @(PathSegment label t :> api)) context $ (\(I server :* _) -> server) <$> delayedServers)
      (taggedSumOfRoutes @t @apis @infos context $ (\(_ :* servers) -> servers) <$> delayedServers)

  hoistTaggedServersWithContext nt (I server :* servers) =
    I (hoistServerWithContext (Proxy @(PathSegment label t :> api)) (Proxy @context) nt server)
      :* hoistTaggedServersWithContext @t @apis @infos @context nt servers

instance
  ( TaggedSumOfApisHasServers
      t
      (GenericRecordFields (mkApiRecord AsApi))
      (GenericRecordFieldInfos (mkApiRecord AsApi))
      context,
    forall m. TaggedSumOfServersFields mkApiRecord m
  ) =>
  HasServer (TaggedSumOfApis t mkApiRecord) context
  where
  type ServerT (TaggedSumOfApis t mkApiRecord) m = TaggedSumOfServers (mkApiRecord (AsServerT m))

  route _ context delayedServer =
    taggedSumOfRoutes @t
      @(GenericRecordFields (mkApiRecord AsApi))
      @(GenericRecordFieldInfos (mkApiRecord AsApi))
      context
      (taggedSumOfServersToFields . unTaggedSumOfServers <$> delayedServer)

  hoistServerWithContext _ _ nt servers =
    TaggedSumOfServers
      . taggedSumOfServersFromFields
      . hoistTaggedServersWithContext @t
        @(GenericRecordFields (mkApiRecord AsApi))
        @(GenericRecordFieldInfos (mkApiRecord AsApi))
        @context
        nt
      . taggedSumOfServersToFields
      $ unTaggedSumOfServers servers

instance
  HasOpenApi (UnionOfApis (TaggedApis t (GenericRecordFields (mkApiRecord AsApi)) (GenericRecordFieldInfos (mkApiRecord AsApi)))) =>
  HasOpenApi (TaggedSumOfApis t mkApiRecord)
  where
  toOpenApi _ = toOpenApi $ Proxy @(UnionOfApis (TaggedApis t (GenericRecordFields (mkApiRecord AsApi)) (GenericRecordFieldInfos (mkApiRecord AsApi))))
