{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module DomainDriven.Server.DomainDrivenApi where

import Data.Kind
import Data.OpenApi (OpenApi, prependPath)
import Data.Text qualified as Text
import DomainDriven.Server.Helper.GenericRecord
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

-- | Wrapper around the data structure containing the API and endpoint definitions.
-- The endpoints name in the record will be added to the path. For example:
-- ```
-- data CounterAction model event mode = CounterAction
--     { increaseWith :: mode :- "something" :> ReqBody '[JSON] Int :> Cmd model event Int
--     }
-- ```
-- Will result in a Post endpoint with path "something/increaseWith".
data DomainDrivenApi (model :: Type) (event :: Type) (mkApiRecord :: Type -> Type)

class ApiTagFromLabel (mkApiRecord :: Type -> Type) where
    apiTagFromLabel :: String -> String
    apiTagFromLabel = id

-- | Wrapper around the data structure containing the API and endpoint definitions.
-- This is used to carry the expectation of `HasServer` for `DomainDrivenApi`. i.e.
-- this is what the `ServerT` type family will produce when given a `DomainDrivenApi`.
newtype RecordOfServers (model :: Type) (event :: Type) a = RecordOfServers {unRecordOfServers :: a}
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
    DomainDrivenApiHasServers
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

instance DomainDrivenApiHasServers mkApiRecord '[] '[] context where
    type ServerTs '[] m = '[]
    taggedSumOfRoutes _ _ = StaticRouter mempty mempty
    hoistTaggedServersWithContext _ Nil = Nil

instance
    ( HasServer api context
    , DomainDrivenApiHasServers mkApiRecord apis infos context
    , KnownSymbol label
    , ApiTagFromLabel mkApiRecord
    )
    => DomainDrivenApiHasServers
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
    ( DomainDrivenApiHasServers
        mkApiRecord
        (GenericRecordFields (mkApiRecord AsApi))
        (GenericRecordFieldInfos (mkApiRecord AsApi))
        context
    , forall m. RecordOfServersFields mkApiRecord m
    )
    => HasServer (DomainDrivenApi model event mkApiRecord) context
    where
    type
        ServerT (DomainDrivenApi model event mkApiRecord) m =
            RecordOfServers model event (mkApiRecord (AsServerT m))

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
    DomainDrivenApiHasOpenApi
        (mkApiRecord :: Type -> Type)
        (apis :: [Api])
        (infos :: [FieldInfo])
    where
    domainDrivenApiToOpenApi :: OpenApi

instance DomainDrivenApiHasOpenApi mkApiRecord '[] '[] where
    domainDrivenApiToOpenApi = mempty

instance
    ( KnownSymbol label
    , ApiTagFromLabel mkApiRecord
    , HasOpenApi api
    , DomainDrivenApiHasOpenApi mkApiRecord apis infos
    )
    => DomainDrivenApiHasOpenApi mkApiRecord (api ': apis) ('FieldInfo label ': infos)
    where
    domainDrivenApiToOpenApi =
        prependPath
            (apiTagFromLabel @mkApiRecord $ symbolVal (Proxy @label))
            (toOpenApi (Proxy @api))
            <> domainDrivenApiToOpenApi @mkApiRecord @apis @infos

instance
    DomainDrivenApiHasOpenApi
        mkApiRecord
        (GenericRecordFields (mkApiRecord AsApi))
        (GenericRecordFieldInfos (mkApiRecord AsApi))
    => HasOpenApi (DomainDrivenApi model event mkApiRecord)
    where
    toOpenApi _ =
        domainDrivenApiToOpenApi @mkApiRecord
            @(GenericRecordFields (mkApiRecord AsApi))
            @(GenericRecordFieldInfos (mkApiRecord AsApi))

instance
    ( GHC.Generic (RecordOfServers model event a)
    , GTo a
    , ThrowAll (SOP I (GCode (RecordOfServers model event a)))
    )
    => ThrowAll (RecordOfServers model event a)
    where
    throwAll = gto . throwAll @(SOP I (GCode (RecordOfServers model event a)))
