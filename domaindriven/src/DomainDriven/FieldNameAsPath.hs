{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module DomainDriven.FieldNameAsPath where

import Data.ByteString.Builder qualified as Builder
import Data.Kind
import Data.OpenApi (OpenApi, prependPath)
import Data.Text qualified as Text
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
import Prelude

-- | Wrapper around the data structure containing the API and endpoint definitions.
-- The endpoints name in the record will be added to the path. For example:
-- ```
-- data CounterAction  mode = CounterAction
--     { increaseWith :: mode :- "something" :> ReqBody '[JSON] Int :> Cmd  Int
--     }
-- ```
-- Will result in a Post endpoint with path "something/increaseWith".
data FieldNameAsPathApi (mkApiRecord :: Type -> Type)

class ApiTagFromLabel (mkApiRecord :: Type -> Type) where
    apiTagFromLabel :: String -> String
    apiTagFromLabel = id

-- | Wrapper around the data structure containing the API and endpoint definitions.
-- This is used to carry the expectation of `HasServer` for `FieldNameAsPath`. i.e.
-- this is what the `ServerT` type family will produce when given a `FieldNameAsPath`.
newtype
    FieldNameAsPathServer
        (mkServerRecord :: Type -> Type)
        (m :: Type -> Type) = FieldNameAsPathServer
    { unDomainDrivenServer
        :: mkServerRecord (AsServerT m)
    }

deriving newtype instance
    GHC.Generic (mkServerRecord (AsServerT m))
    => GHC.Generic (FieldNameAsPathServer mkServerRecord m)

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
    FieldNamesInPathHasServers
        (mkApiRecord :: Type -> Type)
        (apis :: [Type])
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

instance FieldNamesInPathHasServers mkApiRecord '[] '[] context where
    type ServerTs '[] m = '[]
    taggedSumOfRoutes _ _ = StaticRouter mempty mempty
    hoistTaggedServersWithContext _ Nil = Nil

instance
    ( HasServer api context
    , FieldNamesInPathHasServers mkApiRecord apis infos context
    , KnownSymbol label
    , ApiTagFromLabel mkApiRecord
    )
    => FieldNamesInPathHasServers
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
    ( FieldNamesInPathHasServers
        mkApiRecord
        (GenericRecordFields (mkApiRecord AsApi))
        (GenericRecordFieldInfos (mkApiRecord AsApi))
        context
    , forall m. DomainDrivenServerFields (mkApiRecord) m
    )
    => HasServer (FieldNameAsPathApi mkApiRecord) context
    where
    type
        ServerT (FieldNameAsPathApi mkApiRecord) m =
            FieldNameAsPathServer mkApiRecord m

    route _ context delayedServer =
        taggedSumOfRoutes @mkApiRecord
            @(GenericRecordFields (mkApiRecord AsApi))
            @(GenericRecordFieldInfos (mkApiRecord AsApi))
            context
            (recordOfServersToFields . unDomainDrivenServer <$> delayedServer)

    hoistServerWithContext _ _ nt servers =
        FieldNameAsPathServer
            . recordOfServersFromFields
            . hoistTaggedServersWithContext @mkApiRecord
                @(GenericRecordFields (mkApiRecord AsApi))
                @(GenericRecordFieldInfos (mkApiRecord AsApi))
                @context
                nt
            . recordOfServersToFields
            $ unDomainDrivenServer servers

class
    FieldNamesInPathHasOpenApi
        (mkApiRecord :: Type -> Type)
        (apis :: [Type])
        (infos :: [FieldInfo])
    where
    domainDrivenApiToOpenApi :: OpenApi

instance FieldNamesInPathHasOpenApi mkApiRecord '[] '[] where
    domainDrivenApiToOpenApi = mempty

instance
    ( KnownSymbol label
    , ApiTagFromLabel mkApiRecord
    , HasOpenApi api
    , FieldNamesInPathHasOpenApi mkApiRecord apis infos
    )
    => FieldNamesInPathHasOpenApi mkApiRecord (api ': apis) ('FieldInfo label ': infos)
    where
    domainDrivenApiToOpenApi =
        prependPath
            (apiTagFromLabel @mkApiRecord $ symbolVal (Proxy @label))
            (toOpenApi (Proxy @api))
            <> domainDrivenApiToOpenApi @mkApiRecord @apis @infos

instance
    FieldNamesInPathHasOpenApi
        mkApiRecord
        (GenericRecordFields (mkApiRecord AsApi))
        (GenericRecordFieldInfos (mkApiRecord AsApi))
    => HasOpenApi (FieldNameAsPathApi mkApiRecord)
    where
    toOpenApi _ =
        domainDrivenApiToOpenApi @mkApiRecord
            @(GenericRecordFields (mkApiRecord AsApi))
            @(GenericRecordFieldInfos (mkApiRecord AsApi))

instance
    ( GHC.Generic (FieldNameAsPathServer mkServerRecord m)
    , GTo (FieldNameAsPathServer mkServerRecord m)
    , ThrowAll (SOP I (GCode (FieldNameAsPathServer mkServerRecord m)))
    )
    => ThrowAll (FieldNameAsPathServer mkServerRecord m)
    where
    throwAll = gto . throwAll @(SOP I (GCode (FieldNameAsPathServer mkServerRecord m)))

class
    FieldNamesInPathHasClients
        (m :: Type -> Type)
        (mkApiRecord :: Type -> Type)
        (apis :: [Type])
        (infos :: [FieldInfo])
    where
    type Clients apis m :: [Type]
    clientsWithRoute :: Request -> NP I (Clients apis m)
    hoistClientsMonad
        :: (forall x. mon x -> mon' x)
        -> NP I (Clients apis mon)
        -> NP I (Clients apis mon')

instance FieldNamesInPathHasClients m mkApiRecord '[] '[] where
    type Clients '[] m = '[]
    clientsWithRoute _ = Nil
    hoistClientsMonad _ Nil = Nil

instance
    ( HasClient m api
    , FieldNamesInPathHasClients m mkApiRecord apis infos
    , KnownSymbol label
    , ApiTagFromLabel mkApiRecord
    )
    => FieldNamesInPathHasClients m mkApiRecord (api ': apis) ('FieldInfo label ': infos)
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
    , FieldNamesInPathHasClients
        m
        mkApiRecord
        (GenericRecordFields (mkApiRecord AsApi))
        (GenericRecordFieldInfos (mkApiRecord AsApi))
    , forall n. DomainDrivenClientFields (mkApiRecord) n
    )
    => HasClient m (FieldNameAsPathApi mkApiRecord)
    where
    type
        Client m (FieldNameAsPathApi mkApiRecord) =
            mkApiRecord (AsClientT m)
    clientWithRoute _ _ =
        recordOfClientsFromFields
            . ( clientsWithRoute @m @mkApiRecord
                    @(GenericRecordFields (mkApiRecord AsApi))
                    @(GenericRecordFieldInfos (mkApiRecord AsApi))
              )
    hoistClientMonad _ _ nt =
        recordOfClientsFromFields
            . hoistClientsMonad @m @mkApiRecord
                @(GenericRecordFields (mkApiRecord AsApi))
                @(GenericRecordFieldInfos (mkApiRecord AsApi))
                nt
            . recordOfClientsToFields

type family GenericRecordFields (record :: Type) :: [Type] where
    GenericRecordFields record = GenericRecordFields' (GCode record)

type family GenericRecordFields' (code :: [[Type]]) :: [Type] where
    GenericRecordFields' '[fields] = fields
    GenericRecordFields' t = TypeError ('ShowType t ':<>: 'Text " is not a record!")

type family GenericRecordFieldInfos (record :: Type) :: [FieldInfo] where
    GenericRecordFieldInfos record = GenericRecordFieldInfos' (GDatatypeInfoOf record)

type family GenericRecordFieldInfos' (info :: DatatypeInfo) :: [FieldInfo] where
    GenericRecordFieldInfos' ('ADT _ _ '[ 'Record _ infos] _) = infos
    GenericRecordFieldInfos' t = TypeError ('ShowType t ':<>: 'Text " is not a record!")
