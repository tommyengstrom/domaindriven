{-# LANGUAGE AllowAmbiguousTypes #-}
-- got it the wrong way around. this should presumably be domaindriven.servant.api
{-# LANGUAGE UndecidableInstances #-}

module DomainDriven.Server.Api where

import Data.Aeson
import Data.Aeson.Key as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types
import Data.Function
import Data.Kind
import Data.OpenApi
import Data.OpenApi.Internal.Schema
import Data.Text qualified as Text
import Data.Typeable
import GHC.TypeLits
import Generics.SOP hiding (fieldName)
import Optics
import Servant.API (JSON, StdMethod (GET, POST), Verb)
import Servant.Client.Core
import Servant.OpenApi
import Prelude

type Cmd model event a = Cmd' model () event (Verb 'POST 200 '[JSON] a)
type CbCmd model event a = CbCmd' model event (Verb 'POST 200 '[JSON] a)
type CbQuery model a = CbQuery' model (Verb 'GET 200 '[JSON] a)
type Query model a = Query' model (Verb 'GET 200 '[JSON] a)

data Cmd' (model :: Type) (index :: Type) (event :: Type) (verb :: Type)

data Query' (model :: Type) (index :: Type) (verb :: Type)

data CbQuery' (model :: Type) (index :: Type) (verb :: Type)

data CbCmd' (model :: Type) (index :: Type) (event :: Type) (verb :: Type)

instance HasOpenApi verb => HasOpenApi (Cmd' model index event verb) where
    toOpenApi _ = toOpenApi $ Proxy @verb

instance HasOpenApi verb => HasOpenApi (CbCmd' model index event verb) where
    toOpenApi _ = toOpenApi $ Proxy @verb

instance HasOpenApi verb => HasOpenApi (Query' model index verb) where
    toOpenApi _ = toOpenApi $ Proxy @verb

instance HasOpenApi verb => HasOpenApi (CbQuery' model index verb) where
    toOpenApi _ = toOpenApi $ Proxy @verb

data NamedField = NamedField Symbol Type

type family FieldTypes (fields :: [NamedField]) :: [Type] where
    FieldTypes '[] = '[]
    FieldTypes ('NamedField _ t ': fields) = t ': FieldTypes fields

newtype Field a = Field a

newtype JsonObject (fields :: [NamedField]) = JsonObject (NP Field (FieldTypes fields))

class ParseFields (fields :: [NamedField]) where
    parseFields :: Object -> Parser (NP Field (FieldTypes fields))

class UnparseFields (fields :: [NamedField]) where
    unparseFields :: NP Field (FieldTypes fields) -> Object

instance UnparseFields '[] where
    unparseFields Nil = mempty

instance
    ( KnownSymbol name
    , ToJSON a
    , UnparseFields fields
    )
    => UnparseFields ('NamedField name a ': fields)
    where
    unparseFields (Field a :* fields) =
        KeyMap.insert (Key.fromString (symbolVal $ Proxy @name)) (toJSON a) $
            unparseFields @fields fields

instance ParseFields '[] where
    parseFields _ = pure Nil

instance
    (ParseFields fields, FromJSON t, KnownSymbol name)
    => ParseFields ('NamedField name (Maybe t) ': fields)
    where
    parseFields o = do
        fields <- parseFields @fields o
        t <- o .:? fromString (symbolVal (Proxy @name))
        pure $ Field t :* fields

instance
    {-# OVERLAPPABLE #-}
    (ParseFields fields, FromJSON t, KnownSymbol name)
    => ParseFields ('NamedField name t ': fields)
    where
    parseFields o = do
        fields <- parseFields @fields o
        t <- o .: fromString (symbolVal (Proxy @name))
        pure $ Field t :* fields

instance ParseFields fields => FromJSON (JsonObject fields) where
    parseJSON = withObject "JsonObject" $ \o -> JsonObject <$> parseFields @fields o

instance ToSchema (JsonObject '[]) where
    declareNamedSchema _ = pure . unnamed $ mempty & #type ?~ OpenApiObject

instance
    ( Typeable (JsonObject ('NamedField name t ': fields))
    , ToSchema (JsonObject fields)
    , ToSchema t
    , KnownSymbol name
    )
    => ToSchema (JsonObject ('NamedField name t ': fields))
    where
    declareNamedSchema _ = do
        NamedSchema _ subSchema <- declareNamedSchema (Proxy @(JsonObject fields))
        let fieldName = Text.pack $ symbolVal (Proxy @name)
        fieldSchemaRef <- declareSchemaRef (Proxy @t)
        pure . NamedSchema Nothing $
            subSchema
                & #properties % at fieldName ?~ fieldSchemaRef
                & if isOptional @t
                    then id
                    else #required %~ (++ [fieldName])

instance UnparseFields fields => ToJSON (JsonObject fields) where
    toJSON (JsonObject fields) = Object $ unparseFields @fields fields

class IsOptional t where
    isOptional :: Bool

instance IsOptional (Maybe t) where
    isOptional = True

instance {-# OVERLAPPABLE #-} IsOptional t where
    isOptional = False

instance
    {-# OVERLAPPING #-}
    HasClient m (Verb method status cts ret)
    => HasClient m (Cmd' model index event (Verb method status cts ret))
    where
    type Client m (Cmd' model index event (Verb method status cts ret)) = m ret
    clientWithRoute pm _ = clientWithRoute pm (Proxy @(Verb method status cts ret))
    hoistClientMonad _ _ f ma = f ma

instance
    {-# OVERLAPPING #-}
    HasClient m (Verb method status cts ret)
    => HasClient m (CbCmd' model index event (Verb method status cts ret))
    where
    type Client m (CbCmd' model index event (Verb method status cts ret)) = m ret
    clientWithRoute pm _ = clientWithRoute pm (Proxy @(Verb method status cts ret))
    hoistClientMonad _ _ f ma = f ma

instance
    {-# OVERLAPPING #-}
    HasClient m (Verb method status cts ret)
    => HasClient m (Query' model index (Verb method status cts ret))
    where
    type Client m (Query' model index (Verb method status cts ret)) = m ret
    clientWithRoute pm _ = clientWithRoute pm (Proxy @(Verb method status cts ret))
    hoistClientMonad _ _ f ma = f ma

instance
    {-# OVERLAPPING #-}
    HasClient m (Verb method status cts ret)
    => HasClient m (CbQuery' model index (Verb method status cts ret))
    where
    type Client m (CbQuery' model index (Verb method status cts ret)) = m ret
    clientWithRoute pm _ = clientWithRoute pm (Proxy @(Verb method status cts ret))
    hoistClientMonad _ _ f ma = f ma
