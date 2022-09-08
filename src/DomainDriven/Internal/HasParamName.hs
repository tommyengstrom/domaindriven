{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}

module DomainDriven.Internal.HasParamName where

import           Data.Aeson
import qualified Data.HashMap.Strict                          as HM
import qualified Data.Map                                     as M
import           Data.OpenApi
import           Data.Set                       ( Set )
import qualified Data.Text                                    as T
import           Data.Text                      ( Text )
import           Data.Time
import           Data.Vector                    ( Vector )
import           DomainDriven.Internal.Text
import           GHC.Generics
import           Prelude

class HasParamName t where
  paramName :: Text
  default paramName :: (Generic t, GHasFieldName (Rep t)) => Text
  paramName = gParamName $ from (undefined :: t)

instance HasParamName Int where
    paramName = "int"

instance HasParamName Double where
    paramName = "double"

instance HasParamName Char where
    paramName = "char"

instance HasParamName Text where
    paramName = "text"

instance HasParamName Bool where
    paramName = "bool"

instance HasParamName Day where
    paramName = "day"

instance HasParamName UTCTime where
    paramName = "utcTime"

instance
  ( FromJSONKey k,ToJSONKey k,Ord k,HasParamName v,ToSchema k) =>
  HasParamName (M.Map k v)
  where
    paramName = "mapOf" `camelAppend` paramName @v

instance
  ( FromJSONKey k,ToJSONKey k,Ord k,HasParamName v,ToSchema k) =>
  HasParamName (HM.HashMap k v)
  where
    paramName = "mapOf" `camelAppend` paramName @v

instance (Ord v, HasParamName v) => HasParamName (Set v) where
    paramName = "setOf" `camelAppend` paramName @v

instance {-# OVERLAPPABLE #-} HasParamName v => HasParamName [v] where
    paramName = "listOf" `camelAppend` paramName @v

instance {-# OVERLAPPING #-} HasParamName String where
    paramName = "string"

instance HasParamName v => HasParamName (Vector v) where
    paramName = "vectorOf" `camelAppend` paramName @v

instance HasParamName v => HasParamName (Maybe v) where
    paramName = paramName @v

class GHasFieldName t where
  gParamName :: t x -> Text

instance Datatype c => GHasFieldName (M1 i c f) where
    gParamName = T.pack . lowerFirst . datatypeName
