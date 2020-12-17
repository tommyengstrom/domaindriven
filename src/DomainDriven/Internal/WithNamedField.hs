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
import           Control.Applicative

gToNamedJson :: (GNamedToJSON (Rep a), Generic a) => a -> Value
gToNamedJson = Object . HM.fromList . gToTupleList [] . from

type UsedName = Text

class GNamedToJSON a where
    gToTupleList :: [UsedName] -> a x -> [(Text, Value)]

instance (GNamedToJSON f, Datatype d) => GNamedToJSON (M1 D d f) where
    gToTupleList used = gToTupleList used . unM1

instance (GNamedToJSON f, Constructor c) => GNamedToJSON (M1 C c f) where
    gToTupleList used a =
        [("tag", String . T.pack $ conName a)] <> gToTupleList used (unM1 a)

instance (JsonFieldName t) => GNamedToJSON (M1 S c (Rec0 t)) where
    gToTupleList used a = [(actualFieldName used $ fieldName @t, toJSON . unK1 $ unM1 a)]

instance GNamedToJSON U1 where
    gToTupleList _ U1 = []

instance (GNamedToJSON a, GNamedToJSON b) => GNamedToJSON (a :*: b) where
    gToTupleList used (a :*: b) =
        let p1 = gToTupleList used a
            p2 = gToTupleList (fst <$> p1) b
        in  p1 <> p2

instance (GNamedToJSON a, GNamedToJSON b) => GNamedToJSON (a :+: b) where
    gToTupleList used = \case
        L1 a -> gToTupleList used a
        R1 a -> gToTupleList used a


actualFieldName :: [UsedName] -> Text -> Text
actualFieldName used fName = fName <> case length (filter (== fName) used) of
    0 -> ""
    i -> "_" <> (T.pack $ show (i + 1))

-----------------------------------------------------------------------------

lookupKey :: Text -> Value -> Parser Value
lookupKey k = \case
    Object o -> maybe (fail $ "No key " <> show k) pure $ HM.lookup k o
    _        -> fail $ "Expected " <> show k <> " to be an object."

gParseNamedJson :: (GNamedFromJSON (Rep a), Generic a) => Value -> Parser a
gParseNamedJson = fmap to . gNamedFromJSON []

class GNamedFromJSON a where
  gNamedFromJSON :: [UsedName] -> Value -> Parser (a x)

instance GNamedFromJSON p => GNamedFromJSON (M1 D f p) where
    gNamedFromJSON used v = M1 <$> gNamedFromJSON used v

instance (Constructor f, GNamedFromJSON p) => GNamedFromJSON (M1 C f p) where
    gNamedFromJSON used v = do
        c   <- M1 <$> gNamedFromJSON used v
        tag <- lookupKey "tag" v
        case tag of
            String t | T.unpack t == conName c -> pure c
            _ -> fail "Unknown tag"

instance GNamedFromJSON U1 where
    gNamedFromJSON _ _ = pure U1

instance (GNamedFromJSON a, GNamedFromJSON b) => GNamedFromJSON (a :+: b) where
    gNamedFromJSON used vals =
        L1 <$> gNamedFromJSON used vals <|> R1 <$> gNamedFromJSON used vals


instance (GNamedFromJSON a, GNamedFromJSON b) => GNamedFromJSON (a :*: b) where
    gNamedFromJSON used vals =
        (:*:) <$> gNamedFromJSON [] vals <*> gNamedFromJSON [] vals

instance JsonFieldName t => GNamedFromJSON (M1 S c (Rec0 t)) where
    gNamedFromJSON used vals = do
        v <- lookupKey (actualFieldName used $ fieldName @t) vals
        M1 . K1 <$> (parseJSON @t) v

------------------------------------------------------------------------------


data Duplicated = Duplicated Int Int
    deriving (Show, Generic)

instance ToJSON Duplicated where
    toJSON = gToNamedJson
