{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}

module DomainDriven.Internal.HasFieldName where

import           Data.Aeson
import           Data.OpenApi
import           GHC.Generics
import           Prelude
import qualified Data.Text                                    as T
import           Data.Text                      ( Text )
import           Data.Set                       ( Set )
import           Data.Vector                    ( Vector )
import qualified Data.Map                                     as M
import qualified Data.HashMap.Strict                          as HM
import           Data.Time
import           DomainDriven.Internal.Text

class HasFieldName t where
  fieldName :: Text
  default fieldName :: (Generic t, GHasFieldName (Rep t)) => Text
  fieldName = gfieldName $ from (undefined :: t)

instance HasFieldName Int where
    fieldName = "int"

instance HasFieldName Double where
    fieldName = "double"

instance HasFieldName Text where
    fieldName = "text"

instance HasFieldName Bool where
    fieldName = "bool"

instance HasFieldName Day where
    fieldName = "day"

instance HasFieldName UTCTime where
    fieldName = "utcTime"

instance
  ( FromJSONKey k,ToJSONKey k,Ord k,HasFieldName v,ToSchema k) =>
  HasFieldName (M.Map k v)
  where
    fieldName = "mapOf" `camelAppend` fieldName @v

instance
  ( FromJSONKey k,ToJSONKey k,Ord k,HasFieldName v,ToSchema k) =>
  HasFieldName (HM.HashMap k v)
  where
    fieldName = "mapOf" `camelAppend` fieldName @v

instance (Ord v, HasFieldName v) => HasFieldName (Set v) where
    fieldName = "setOf" `camelAppend` fieldName @v

instance {-# OVERLAPPABLE #-} HasFieldName v => HasFieldName [v] where
    fieldName = "listOf" `camelAppend` fieldName @v

instance {-# OVERLAPPING #-} HasFieldName String where
    fieldName = "string"

instance HasFieldName v => HasFieldName (Vector v) where
    fieldName = "vectorOf" `camelAppend` fieldName @v

instance HasFieldName v => HasFieldName (Maybe v) where
    fieldName = fieldName @v

class GHasFieldName t where
  gfieldName :: t x -> Text

instance Datatype c => GHasFieldName (M1 i c f) where
    gfieldName = T.pack . lowerFirst . datatypeName
