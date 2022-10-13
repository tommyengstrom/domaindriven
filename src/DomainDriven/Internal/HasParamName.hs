{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}

module DomainDriven.Internal.HasParamName where

import           Data.Aeson
import qualified Data.HashMap.Strict                          as HM
import qualified Data.Map                                     as M
import           Data.OpenApi            hiding ( ParamName )
import           Data.Proxy
import           Data.Set                       ( Set )
import qualified Data.Text                                    as T
import           Data.Text                      ( Text )
import           Data.Time
import           Data.Vector                    ( Vector )
import           DomainDriven.Internal.Text
import           GHC.Generics
import           GHC.TypeLits
import           Prelude

class HasParamName t where
  type ParamName t :: Symbol
  paramName :: Text
  -- default paramName :: (Generic t, GHasParamName (Rep t)) => Text
  -- paramName = gParamName $ from (undefined :: t)
  paramName = T.pack $ symbolVal (Proxy @(ParamName Int))

instance HasParamName Int where
    type ParamName Int = "int"

instance HasParamName Double where
    type ParamName Double = "double"

instance HasParamName Char where
    type ParamName Char = "char"

instance HasParamName Text where
    type ParamName Text = "text"

instance HasParamName Bool where
    type ParamName Bool = "bool"

instance HasParamName Day where
    type ParamName Day = "day"

instance HasParamName UTCTime where
    type ParamName UTCTime = "utcTime"

instance
  ( FromJSONKey k,ToJSONKey k,Ord k,HasParamName v,ToSchema k) =>
  HasParamName (M.Map k v)
  where
    paramName = "mapOf" `camelAppendT` paramName @v

instance
  ( FromJSONKey k,ToJSONKey k,Ord k,HasParamName v,ToSchema k) =>
  HasParamName (HM.HashMap k v)
  where
    paramName = "mapOf" `camelAppendT` paramName @v

instance (Ord v, HasParamName v) => HasParamName (Set v) where
    paramName = "setOf" `camelAppendT` paramName @v

instance {-# OVERLAPPABLE #-} HasParamName v => HasParamName [v] where
    paramName = "listOf" `camelAppendT` paramName @v

instance {-# OVERLAPPING #-} HasParamName String where
    paramName = "string"

instance HasParamName v => HasParamName (Vector v) where
    paramName = "vectorOf" `camelAppendT` paramName @v

instance HasParamName v => HasParamName (Maybe v) where
    paramName = paramName @v

class GHasParamName t where
  gParamName :: t x -> Text

instance Datatype c => GHasParamName (M1 i c f) where
    gParamName = T.pack . lowerFirst . datatypeName
