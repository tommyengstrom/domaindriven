{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}

module DomainDriven.Internal.HasFieldName where

import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Vector (Vector)
import DomainDriven.Internal.Text
import GHC.Generics
import Prelude

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
    HasFieldName v
    => HasFieldName (M.Map k v)
    where
    fieldName = "mapOf" `camelAppendT` fieldName @v

instance
    HasFieldName v
    => HasFieldName (HM.HashMap k v)
    where
    fieldName = "mapOf" `camelAppendT` fieldName @v

instance HasFieldName v => HasFieldName (Set v) where
    fieldName = "setOf" `camelAppendT` fieldName @v

instance {-# OVERLAPPABLE #-} HasFieldName v => HasFieldName [v] where
    fieldName = "listOf" `camelAppendT` fieldName @v

instance {-# OVERLAPPING #-} HasFieldName String where
    fieldName = "string"

instance HasFieldName v => HasFieldName (Vector v) where
    fieldName = "vectorOf" `camelAppendT` fieldName @v

instance HasFieldName v => HasFieldName (Maybe v) where
    fieldName = fieldName @v

class GHasFieldName t where
    gfieldName :: t x -> Text

instance Datatype c => GHasFieldName (M1 i c f) where
    gfieldName = T.pack . lowerFirst . datatypeName
