{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}

module DomainDriven.Internal.JsonFieldName where

import           Data.Aeson
import           Data.Char                      ( toLower )
import           Data.OpenApi
import           GHC.Generics
import           RIO
import qualified RIO.Text                                     as T
import           RIO.Time
import           GHC.Tuple

class (FromJSON t, ToJSON t, ToSchema t) => JsonFieldName t where
  fieldName :: Text
  default fieldName :: (Generic t, GJsonFieldName (Rep t)) => Text
  fieldName = gfieldName $ from (undefined :: t)

instance ToJSON a => ToJSON (Unit a) where
    toJSON (Unit a) = toJSON a

instance FromJSON a => FromJSON (Unit a) where
    parseJSON = fmap Unit . parseJSON

instance ToSchema a => ToSchema (Unit a) where
    declareNamedSchema _ = declareNamedSchema (Proxy @a)

instance JsonFieldName a => JsonFieldName (Unit a) where
    fieldName = fieldName @a

instance JsonFieldName Int where
    fieldName = "int"

instance JsonFieldName Double where
    fieldName = "double"

instance JsonFieldName Text where
    fieldName = "text"

instance JsonFieldName Bool where
    fieldName = "bool"

instance JsonFieldName Day where
    fieldName = "day"

instance JsonFieldName UTCTime where
    fieldName = "utcTime"

instance
  ( FromJSONKey k,ToJSONKey k,Ord k,JsonFieldName v,ToSchema k) =>
  JsonFieldName (Map k v)
  where
    fieldName = "mapOf" <> fieldName @v

instance (Ord v, JsonFieldName v) => JsonFieldName (Set v) where
    fieldName = "setOf" <> fieldName @v

instance {-# OVERLAPPABLE #-} JsonFieldName v => JsonFieldName [v] where
    fieldName = "listOf" <> fieldName @v

instance {-# OVERLAPPING #-} JsonFieldName String where
    fieldName = "string"

instance JsonFieldName v => JsonFieldName (Vector v) where
    fieldName = "vectorOf" <> fieldName @v

instance JsonFieldName v => JsonFieldName (Maybe v) where
    fieldName = "m" <> fieldName @v

class GJsonFieldName t where
  gfieldName :: t x -> Text

instance Datatype c => GJsonFieldName (M1 i c f) where
    gfieldName = T.pack . lowerFirst . datatypeName
      where
        lowerFirst :: String -> String
        lowerFirst = \case
            []     -> []
            s : ss -> toLower s : ss
