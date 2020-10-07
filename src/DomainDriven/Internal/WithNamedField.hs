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

class GNamedToJSON a where
    gNamedToJSON :: a x -> [(Text, Value)]

instance (GNamedToJSON f, Datatype d) => GNamedToJSON (M1 D d f) where
    gNamedToJSON = gNamedToJSON . unM1

instance (GNamedToJSON f, Constructor c) => GNamedToJSON (M1 C c f) where
    gNamedToJSON a = [("tag", String . T.pack $ conName a)] <> gNamedToJSON (unM1 a)

instance (JsonFieldName t) => GNamedToJSON (M1 S c (Rec0 t)) where
    gNamedToJSON a = [(fieldName @t, toJSON . unK1 $ unM1 a)]

instance GNamedToJSON U1 where
    gNamedToJSON U1 = []

instance (GNamedToJSON a, GNamedToJSON b) => GNamedToJSON (a :*: b) where
    gNamedToJSON (a :*: b) = gNamedToJSON a <> gNamedToJSON b

instance (GNamedToJSON a, GNamedToJSON b) => GNamedToJSON (a :+: b) where
    gNamedToJSON = \case
        L1 a -> gNamedToJSON a
        R1 a -> gNamedToJSON a

------------------------------------------------------------------------------------------
-- -- It is possible to to go [(Text, Value)] from Value using:
-- tocrap :: Value -> [(Text, Value)]
-- tocrap = \case
--   Object o -> toList o
--   _ -> []

class GNamedFromJSON a where
  gNamedFromJSON :: [(Text, Value)] -> Parser (a x)

instance GNamedFromJSON p => GNamedFromJSON (M1 D f p) where
  gNamedFromJSON vals = M1 <$> gNamedFromJSON vals

instance (Constructor f, GNamedFromJSON p) => GNamedFromJSON (M1 C f p) where
  gNamedFromJSON vals = do
      c <- M1 <$> gNamedFromJSON vals
      case filter ((== "tag") . fst) vals of
          (_,String tag):_  -> if T.unpack tag ==conName c then pure c else fail "wrong tag"
          _ -> fail "No tag field found"

instance GNamedFromJSON U1 where
  gNamedFromJSON _ = pure U1

instance (GNamedFromJSON a, GNamedFromJSON b) => GNamedFromJSON (a :+: b) where
  gNamedFromJSON vals = L1 <$> gNamedFromJSON vals
                    <|> R1 <$> gNamedFromJSON vals


instance (GNamedFromJSON a, GNamedFromJSON b) => GNamedFromJSON (a :*: b) where
  gNamedFromJSON vals = (:*:) <$> gNamedFromJSON vals <*> gNamedFromJSON vals

instance JsonFieldName t => GNamedFromJSON (M1 S c (Rec0 t)) where
  gNamedFromJSON vals = case filter ((== fieldName @t) . fst) vals of
      (_, v):_ -> M1 . K1 <$> (parseJSON @t) v
      [] -> fail $ "No field named " <> show (fieldName @t)

--But you can also fail, so you need
-- [(Text, Value)] -> Either Text (a x)
--From there, I'd define the leaf:
--gNamedFromJson vals =
--  case List.lookup (fieldName @t) vals of
--    Nothing -> Left "nope"
--    Just value ->
--      parseJSON value
------------------------------------------------------------------------------------------

------------------------

--class Generic a => GNamedFromJSON a where
--    gNamedParseJSON :: Rep a x -> Value -> Parser a
--
---- instance (Datatype d) => GNamedFromJSON (M1 D d f) where
----   gNamedParseJSON r = gNamedParseJSON (unM1 r)
--
--instance (Constructor c, JsonFieldName t) => GNamedFromJSON (M1 S c (Rec0 t) x) where
--  gNamedParseJSON a = \case
--    Object o -> do
--        o .: T.pack (conName a)
--    x -> fail $ "expected and object but got: " <> show x
--------- Testing it ----------------
data Apa = Apan
    deriving (Show, Eq, Ord, Generic)

instance FromJSON Apa where
  parseJSON _ = to <$> gNamedFromJSON []

data Bulle = Middag
    | Beer Int Text
    | Kaffe Text
    deriving (Show, Eq, Ord, Generic)

data Fluff = Fluff Int Text
    deriving (Show, Eq, Ord, Generic)

instance ToJSON Fluff where
    toJSON = Object . HM.fromList . gNamedToJSON . from

instance FromJSON Fluff where
  parseJSON = \case
    Object o -> to <$> gNamedFromJSON (HM.toList o)

instance FromJSON Bulle where
  parseJSON = \case
    Object o -> to <$> gNamedFromJSON (HM.toList o)

instance ToJSON Apa where
    toJSON = Object . HM.fromList . gNamedToJSON . from

instance ToJSON Bulle where
    toJSON = Object . HM.fromList . gNamedToJSON . from


test :: IO ()
test = do
    print $ encode $ Apan
    print $ encode Middag
    print $ encode $ Beer 2 "bulle"
    print $ encode $ Kaffe "kaffe!"

bulleRep :: Rep Bulle x
bulleRep = from $ Kaffe "hejsan"
