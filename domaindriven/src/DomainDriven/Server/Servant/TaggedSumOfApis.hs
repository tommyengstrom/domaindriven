{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module DomainDriven.Server.Servant.TaggedSumOfApis where

import Data.Kind
import Data.OpenApi (OpenApi, prependPath)
import Data.Text qualified as Text
import DomainDriven.Server.Servant.Helper.GenericRecord
import GHC.Generics qualified as GHC
import GHC.TypeLits
import Generics.SOP
    ( I (..)
    , NP (..)
    , NS (..)
    , SOP (..)
    , unSOP
    )
import Generics.SOP.GGP
import Generics.SOP.Type.Metadata
import Servant
import Servant.Auth.Internal.ThrowAll.SOP ()
import Servant.Auth.Server.Internal.ThrowAll
import Servant.OpenApi
import Servant.Server.Generic
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.Router
import Prelude

type Api = Type

data TaggedSumOfApis (mkApiRecord :: Type -> Type)

class ApiTagFromLabel (mkApiRecord :: Type -> Type) where
    apiTagFromLabel :: String -> String
    apiTagFromLabel = id

newtype RecordOfServers a = RecordOfServers {unRecordOfServers :: a}
    deriving newtype (GHC.Generic)

class RecordOfServersFields (mkApiRecord :: Type -> Type) (m :: Type -> Type) where
    recordOfServersFromFields
        :: NP I (ServerTs (GenericRecordFields (mkApiRecord AsApi)) m)
        -> mkApiRecord (AsServerT m)
    recordOfServersToFields
        :: mkApiRecord (AsServerT m)
        -> NP I (ServerTs (GenericRecordFields (mkApiRecord AsApi)) m)

instance
    ( GHC.Generic (mkApiRecord (AsServerT m))
    , GCode (mkApiRecord (AsServerT m))
        ~ '[ServerTs (GenericRecordFields (mkApiRecord AsApi)) m]
    , GFrom (mkApiRecord (AsServerT m))
    , GTo (mkApiRecord (AsServerT m))
    )
    => RecordOfServersFields mkApiRecord m
    where
    recordOfServersFromFields = gto . SOP . Z
    recordOfServersToFields x = case unSOP $ gfrom x of
        Z servers -> servers
        S y -> case y of {}

class
    TaggedSumOfApisHasServers
        (mkApiRecord :: Type -> Type)
        (apis :: [Api])
        (infos :: [FieldInfo])
        (context :: [Type])
    where
    type ServerTs apis (m :: Type -> Type) :: [Type]
    taggedSumOfRoutes
        :: Context context
        -> Delayed env (NP I (ServerTs apis Handler))
        -> Router env
    hoistTaggedServersWithContext
        :: (forall x. m x -> n x)
        -> NP I (ServerTs apis m)
        -> NP I (ServerTs apis n)

instance TaggedSumOfApisHasServers mkApiRecord '[] '[] context where
    type ServerTs '[] m = '[]
    taggedSumOfRoutes _ _ = StaticRouter mempty mempty
    hoistTaggedServersWithContext _ Nil = Nil

instance
    ( HasServer api context
    , TaggedSumOfApisHasServers mkApiRecord apis infos context
    , KnownSymbol label
    , ApiTagFromLabel mkApiRecord
    )
    => TaggedSumOfApisHasServers
        mkApiRecord
        (api ': apis)
        ('FieldInfo label ': infos)
        context
    where
    type ServerTs (api ': apis) m = ServerT api m ': ServerTs apis m

    taggedSumOfRoutes context delayedServers =
        choice
            ( pathRouter
                (Text.pack $ apiTagFromLabel @mkApiRecord $ symbolVal (Proxy @label))
                $ route (Proxy @api) context
                $ (\(I server :* _) -> server) <$> delayedServers
            )
            ( taggedSumOfRoutes @mkApiRecord @apis @infos context $
                (\(_ :* servers) -> servers) <$> delayedServers
            )

    hoistTaggedServersWithContext nt (I server :* servers) =
        I (hoistServerWithContext (Proxy @api) (Proxy @context) nt server)
            :* hoistTaggedServersWithContext @mkApiRecord @apis @infos @context nt servers

instance
    ( TaggedSumOfApisHasServers
        mkApiRecord
        (GenericRecordFields (mkApiRecord AsApi))
        (GenericRecordFieldInfos (mkApiRecord AsApi))
        context
    , forall m. RecordOfServersFields mkApiRecord m
    )
    => HasServer (TaggedSumOfApis mkApiRecord) context
    where
    type
        ServerT (TaggedSumOfApis mkApiRecord) m =
            RecordOfServers (mkApiRecord (AsServerT m))

    route _ context delayedServer =
        taggedSumOfRoutes @mkApiRecord
            @(GenericRecordFields (mkApiRecord AsApi))
            @(GenericRecordFieldInfos (mkApiRecord AsApi))
            context
            (recordOfServersToFields . unRecordOfServers <$> delayedServer)

    hoistServerWithContext _ _ nt servers =
        RecordOfServers
            . recordOfServersFromFields
            . hoistTaggedServersWithContext @mkApiRecord
                @(GenericRecordFields (mkApiRecord AsApi))
                @(GenericRecordFieldInfos (mkApiRecord AsApi))
                @context
                nt
            . recordOfServersToFields
            $ unRecordOfServers servers

class
    TaggedSumOfApisHasOpenApi
        (mkApiRecord :: Type -> Type)
        (apis :: [Api])
        (infos :: [FieldInfo])
    where
    taggedSumOfApisToOpenApi :: OpenApi

instance TaggedSumOfApisHasOpenApi mkApiRecord '[] '[] where
    taggedSumOfApisToOpenApi = mempty

instance
    ( KnownSymbol label
    , ApiTagFromLabel mkApiRecord
    , HasOpenApi api
    , TaggedSumOfApisHasOpenApi mkApiRecord apis infos
    )
    => TaggedSumOfApisHasOpenApi mkApiRecord (api ': apis) ('FieldInfo label ': infos)
    where
    taggedSumOfApisToOpenApi =
        prependPath
            (apiTagFromLabel @mkApiRecord $ symbolVal (Proxy @label))
            (toOpenApi (Proxy @api))
            <> taggedSumOfApisToOpenApi @mkApiRecord @apis @infos

instance
    TaggedSumOfApisHasOpenApi
        mkApiRecord
        (GenericRecordFields (mkApiRecord AsApi))
        (GenericRecordFieldInfos (mkApiRecord AsApi))
    => HasOpenApi (TaggedSumOfApis mkApiRecord)
    where
    toOpenApi _ =
        taggedSumOfApisToOpenApi @mkApiRecord
            @(GenericRecordFields (mkApiRecord AsApi))
            @(GenericRecordFieldInfos (mkApiRecord AsApi))

instance
    ( GHC.Generic (RecordOfServers a)
    , GTo a
    , ThrowAll (SOP I (GCode (RecordOfServers a)))
    )
    => ThrowAll (RecordOfServers a)
    where
    throwAll = gto . throwAll @(SOP I (GCode (RecordOfServers a)))
