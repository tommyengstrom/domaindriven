-- got it the wrong way around. this should presumably be domaindriven.servant.api
{-# LANGUAGE AllowAmbiguousTypes #-}

module DomainDriven.Server.Servant.Api where

import Data.Aeson
import Data.Aeson.Key
import Data.Aeson.Types
import Data.Kind
import GHC.TypeLits
import Generics.SOP
import Servant
import Prelude

data Cmd (model :: Type) (event :: Type) (verb :: Type)

data Query (model :: Type) (verb :: Type)

data CbQuery (model :: Type) (verb :: Type)

data CbCmd (model :: Type) (event :: Type) (verb :: Type)

data NamedField = NamedField Symbol Type

type family FieldTypes (fields :: [NamedField]) :: [Type] where
  FieldTypes '[] = '[]
  FieldTypes ('NamedField _ t ': fields) = t ': FieldTypes fields

newtype Field a = Field a

newtype JsonObject (fields :: [NamedField]) = JsonObject (NP Field (FieldTypes fields))

type TestApi = ReqBody '[JSON] (JsonObject '[ 'NamedField "asd" Int, 'NamedField "bsd" Int])

class ParseFields (fields :: [NamedField]) where
  parseFields :: Object -> Parser (NP Field (FieldTypes fields))

instance ParseFields '[] where
  parseFields _ = pure Nil

instance (ParseFields fields, FromJSON t, KnownSymbol name) => ParseFields ('NamedField name (Maybe t) ': fields) where
  parseFields o = do
    fields <- parseFields @fields o
    t <- o .:? fromString (symbolVal (Proxy @name))
    pure $ Field t :* fields

instance (ParseFields fields, FromJSON t, KnownSymbol name) => ParseFields ('NamedField name t ': fields) where
  parseFields o = do
    fields <- parseFields @fields o
    t <- o .: fromString (symbolVal (Proxy @name))
    pure $ Field t :* fields

instance ParseFields fields => FromJSON (JsonObject fields) where
  parseJSON = withObject "JsonObject" $ \o -> JsonObject <$> parseFields @fields o
