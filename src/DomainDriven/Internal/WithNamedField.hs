{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DefaultSignatures #-}

module DomainDriven.Internal.WithNamedField where

import           Prelude
import           DomainDriven.Internal.JsonFieldName
import           GHC.Generics
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text                      ( Text )
import qualified Data.Text                                    as T
import qualified Data.HashMap.Strict                          as HM
import Control.Applicative

gParseNamedJson :: (GNamedFromJSON (Rep a), Generic a) => Value -> Parser a
gParseNamedJson = fmap to . gNamedFromJSON

gToNamedJson :: (GNamedToJSON (Rep a), Generic a) => a -> Value
gToNamedJson = Object . HM.fromList . gToTupleList . from

class GNamedToJSON a where
    gToTupleList :: a x -> [(Text, Value)]

instance (GNamedToJSON f, Datatype d) => GNamedToJSON (M1 D d f) where
    gToTupleList = gToTupleList . unM1

instance (GNamedToJSON f, Constructor c) => GNamedToJSON (M1 C c f) where
    gToTupleList a = [("tag", String . T.pack $ conName a)] <> gToTupleList (unM1 a)

instance (JsonFieldName t) => GNamedToJSON (M1 S c (Rec0 t)) where
    gToTupleList a = [(fieldName @t, toJSON . unK1 $ unM1 a)]

instance GNamedToJSON U1 where
    gToTupleList U1 = []

instance (GNamedToJSON a, GNamedToJSON b) => GNamedToJSON (a :*: b) where
    gToTupleList (a :*: b) = gToTupleList a <> gToTupleList b

instance (GNamedToJSON a, GNamedToJSON b) => GNamedToJSON (a :+: b) where
    gToTupleList = \case
        L1 a -> gToTupleList a
        R1 a -> gToTupleList a


lookupKey :: Text -> Value -> Parser Value
lookupKey k = \case
    Object o -> maybe (fail $ "No key " <> show k) pure $ HM.lookup k o
    _        -> fail $ "Expected " <> show k <> " to be an object."

class GNamedFromJSON a where
  gNamedFromJSON :: Value -> Parser (a x)

instance GNamedFromJSON p => GNamedFromJSON (M1 D f p) where
  gNamedFromJSON v = M1 <$> gNamedFromJSON v

instance (Constructor f, GNamedFromJSON p) => GNamedFromJSON (M1 C f p) where
  gNamedFromJSON v = do
      c <- M1 <$> gNamedFromJSON v
      tag <- lookupKey "tag" v
      case tag of
          String t | T.unpack t == conName c -> pure c
          _ -> fail "Unknown tag"

instance GNamedFromJSON U1 where
  gNamedFromJSON _ = pure U1

instance (GNamedFromJSON a, GNamedFromJSON b) => GNamedFromJSON (a :+: b) where
  gNamedFromJSON vals = L1 <$> gNamedFromJSON vals
                    <|> R1 <$> gNamedFromJSON vals


instance (GNamedFromJSON a, GNamedFromJSON b) => GNamedFromJSON (a :*: b) where
  gNamedFromJSON vals = (:*:) <$> gNamedFromJSON vals <*> gNamedFromJSON vals

instance JsonFieldName t => GNamedFromJSON (M1 S c (Rec0 t)) where
  gNamedFromJSON vals = do
      v <- lookupKey (fieldName @t) vals
      M1 . K1 <$> (parseJSON @t) v

--------- Testing it ----------------
data Apa = Apan
    deriving (Show, Eq, Ord, Generic)

instance FromJSON Apa where
  parseJSON = gParseNamedJson

data Bulle = Middag
    | Beer Int Text
    | Kaffe Text
    deriving (Show, Eq, Ord, Generic)

data Fluff = Fluff Int Text
    deriving (Show, Eq, Ord, Generic)

instance ToJSON Fluff where
    toJSON = Object . HM.fromList . gToTupleList . from

instance FromJSON Fluff where
  parseJSON = gParseNamedJson

instance FromJSON Bulle where
  parseJSON = gParseNamedJson

instance ToJSON Apa where
    toJSON = Object . HM.fromList . gToTupleList . from

instance ToJSON Bulle where
    toJSON = Object . HM.fromList . gToTupleList . from


test :: IO ()
test = do
    print $ encode $ Apan
    print $ encode Middag
    print $ encode $ Beer 2 "bulle"
    print $ encode $ Kaffe "kaffe!"


bulleRep :: Rep Bulle x
bulleRep = from $ Kaffe "hejsan"
