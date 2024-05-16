{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module DomainDriven.Server.DomainDrivenApi where

import Data.ByteString.Builder qualified as Builder
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
import Servant.Client.Core
import Servant.Client.Generic
import Servant.OpenApi
import Servant.Server.Generic
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.Router
import Servant.Links
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
data
    DomainDrivenApi
        (mkApiRecord :: Type -> Type -> Type -> Type)
        (model :: Type)
        (event :: Type)

class ApiTagFromLabel (mkApiRecord :: Type -> Type -> Type -> Type) where
    apiTagFromLabel :: String -> String
    apiTagFromLabel = id

-- | Wrapper around the data structure containing the API and endpoint definitions.
-- This is used to carry the expectation of `HasServer` for `DomainDrivenApi`. i.e.
-- this is what the `ServerT` type family will produce when given a `DomainDrivenApi`.
newtype
    DomainDrivenServer
        (mkServerRecord :: Type -> Type -> Type -> Type)
        (model :: Type)
        (event :: Type)
        (m :: Type -> Type) = DomainDrivenServer
    { unDomainDrivenServer
        :: mkServerRecord model event (AsServerT m)
    }

deriving newtype instance
    GHC.Generic (mkServerRecord model event (AsServerT m))
    => GHC.Generic (DomainDrivenServer mkServerRecord model event m)

class DomainDrivenServerFields (mkApiRecord :: Type -> Type) (m :: Type -> Type) where
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
    => DomainDrivenServerFields mkApiRecord m
    where
    recordOfServersFromFields = gto . SOP . Z
    recordOfServersToFields x = case unSOP $ gfrom x of
        Z servers -> servers
        S y -> case y of {}

class
    DomainDrivenApiHasServers
        (mkApiRecord :: Type -> Type -> Type -> Type)
        (model :: Type)
        (event :: Type)
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

instance DomainDrivenApiHasServers mkApiRecord model event '[] '[] context where
    type ServerTs '[] m = '[]
    taggedSumOfRoutes _ _ = StaticRouter mempty mempty
    hoistTaggedServersWithContext _ Nil = Nil

instance
    ( HasServer api context
    , DomainDrivenApiHasServers mkApiRecord model event apis infos context
    , KnownSymbol label
    , ApiTagFromLabel mkApiRecord
    )
    => DomainDrivenApiHasServers
        mkApiRecord
        model
        event
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
            ( taggedSumOfRoutes @mkApiRecord @model @event @apis @infos context $
                (\(_ :* servers) -> servers) <$> delayedServers
            )

    hoistTaggedServersWithContext nt (I server :* servers) =
        I (hoistServerWithContext (Proxy @api) (Proxy @context) nt server)
            :* hoistTaggedServersWithContext @mkApiRecord @model @event @apis @infos @context nt servers

instance
    ( DomainDrivenApiHasServers
        mkApiRecord
        model
        event
        (GenericRecordFields (mkApiRecord model event AsApi))
        (GenericRecordFieldInfos (mkApiRecord model event AsApi))
        context
    , forall m. DomainDrivenServerFields (mkApiRecord model event) m
    )
    => HasServer (DomainDrivenApi mkApiRecord model event) context
    where
    type
        ServerT (DomainDrivenApi mkApiRecord model event) m =
            DomainDrivenServer mkApiRecord model event m

    route _ context delayedServer =
        taggedSumOfRoutes @mkApiRecord @model @event
            @(GenericRecordFields (mkApiRecord model event AsApi))
            @(GenericRecordFieldInfos (mkApiRecord model event AsApi))
            context
            (recordOfServersToFields . unDomainDrivenServer <$> delayedServer)

    hoistServerWithContext _ _ nt servers =
        DomainDrivenServer
            . recordOfServersFromFields
            . hoistTaggedServersWithContext @mkApiRecord @model @event
                @(GenericRecordFields (mkApiRecord model event AsApi))
                @(GenericRecordFieldInfos (mkApiRecord model event AsApi))
                @context
                nt
            . recordOfServersToFields
            $ unDomainDrivenServer servers

class
    DomainDrivenApiHasOpenApi
        (mkApiRecord :: Type -> Type -> Type -> Type)
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
        (GenericRecordFields (mkApiRecord model event AsApi))
        (GenericRecordFieldInfos (mkApiRecord model event AsApi))
    => HasOpenApi (DomainDrivenApi mkApiRecord model event)
    where
    toOpenApi _ =
        domainDrivenApiToOpenApi @mkApiRecord
            @(GenericRecordFields (mkApiRecord model event AsApi))
            @(GenericRecordFieldInfos (mkApiRecord model event AsApi))

instance
    ( GHC.Generic (DomainDrivenServer mkServerRecord model event m)
    , GTo (DomainDrivenServer mkServerRecord model event m)
    , ThrowAll (SOP I (GCode (DomainDrivenServer mkServerRecord model event m)))
    )
    => ThrowAll (DomainDrivenServer mkServerRecord model event m)
    where
    throwAll = gto . throwAll @(SOP I (GCode (DomainDrivenServer mkServerRecord model event m)))

class
    DomainDrivenApiHasClients
        (m :: Type -> Type)
        (mkApiRecord :: Type -> Type -> Type -> Type)
        (apis :: [Api])
        (infos :: [FieldInfo])
    where
    type Clients apis m :: [Type]
    clientsWithRoute :: Request -> NP I (Clients apis m)
    hoistClientsMonad
        :: (forall x. mon x -> mon' x)
        -> NP I (Clients apis mon)
        -> NP I (Clients apis mon')

instance DomainDrivenApiHasClients m mkApiRecord '[] '[] where
    type Clients '[] m = '[]
    clientsWithRoute _ = Nil
    hoistClientsMonad _ Nil = Nil

instance
    ( HasClient m api
    , DomainDrivenApiHasClients m mkApiRecord apis infos
    , KnownSymbol label
    , ApiTagFromLabel mkApiRecord
    )
    => DomainDrivenApiHasClients m mkApiRecord (api ': apis) ('FieldInfo label ': infos)
    where
    type Clients (api ': apis) m = Client m api ': Clients apis m

    clientsWithRoute req =
        I
            ( clientWithRoute
                (Proxy @m)
                (Proxy @api)
                ( appendToPath
                    (Builder.stringUtf8 $ apiTagFromLabel @mkApiRecord $ symbolVal (Proxy @label))
                    req
                )
            )
            :* clientsWithRoute @m @mkApiRecord @apis @infos req
    hoistClientsMonad nt (I client :* clients) =
        I (hoistClientMonad (Proxy @m) (Proxy @api) nt client)
            :* hoistClientsMonad @m @mkApiRecord @apis @infos nt clients

class DomainDrivenClientFields (mkApiRecord :: Type -> Type) (m :: Type -> Type) where
    recordOfClientsFromFields
        :: NP I (Clients (GenericRecordFields (mkApiRecord AsApi)) m)
        -> mkApiRecord (AsClientT m)
    recordOfClientsToFields
        :: mkApiRecord (AsClientT m)
        -> NP I (Clients (GenericRecordFields (mkApiRecord AsApi)) m)

instance
    ( GHC.Generic (mkApiRecord (AsClientT m))
    , GCode (mkApiRecord (AsClientT m))
        ~ '[Clients (GenericRecordFields (mkApiRecord AsApi)) m]
    , GFrom (mkApiRecord (AsClientT m))
    , GTo (mkApiRecord (AsClientT m))
    )
    => DomainDrivenClientFields mkApiRecord m
    where
    recordOfClientsFromFields = gto . SOP . Z
    recordOfClientsToFields x = case unSOP $ gfrom x of
        Z servers -> servers
        S y -> case y of {}

instance
    ( RunClient m
    , DomainDrivenApiHasClients
        m
        mkApiRecord
        (GenericRecordFields (mkApiRecord model event AsApi))
        (GenericRecordFieldInfos (mkApiRecord model event AsApi))
    , forall n. DomainDrivenClientFields (mkApiRecord model event) n
    )
    => HasClient m (DomainDrivenApi mkApiRecord model event)
    where
    type
        Client m (DomainDrivenApi mkApiRecord model event) =
            mkApiRecord model event (AsClientT m)
    clientWithRoute _ _ =
        recordOfClientsFromFields
            . ( clientsWithRoute @m @mkApiRecord
                    @(GenericRecordFields (mkApiRecord model event AsApi))
                    @(GenericRecordFieldInfos (mkApiRecord model event AsApi))
              )
    hoistClientMonad _ _ nt =
        recordOfClientsFromFields
            . hoistClientsMonad @m @mkApiRecord
                @(GenericRecordFields (mkApiRecord model event AsApi))
                @(GenericRecordFieldInfos (mkApiRecord model event AsApi))
                nt
            . recordOfClientsToFields
-- instance
--   ( HasLink (ToServantApi routes)
--   , forall a. GLink routes a
--   , ErrorIfNoGeneric routes
--   ) => HasLink (NamedRoutes routes) where
--
--   type MkLink (NamedRoutes routes) a = routes (AsLink a)
--
--   toLink
--     :: forall a. (Link -> a)
--     -> Proxy (NamedRoutes routes)
--     -> Link
--     -> routes (AsLink a)
--
--   toLink toA _ l = case gLinkProof @routes @a of
--     Dict -> fromServant $ toLink toA (Proxy @(ToServantApi routes)) l
