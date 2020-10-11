{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}

module DomainDriven.Internal.JsonFieldName where

import           Data.Aeson
import           Data.Char                      ( toLower )
import           Data.Swagger
import           GHC.Generics
import           RIO
import qualified RIO.Text                                     as T
import           RIO.Time

class (FromJSON t, ToJSON t, ToSchema t) => JsonFieldName t where
  fieldName :: Text
  default fieldName :: (Generic t, GJsonFieldName (Rep t)) => Text
  fieldName = gfieldName $ from (undefined :: t)

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
