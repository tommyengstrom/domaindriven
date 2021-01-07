{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

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
import           Control.Monad.State

newtype WithNamedField a = WithNamedField a

instance (GNamedToJSON (Rep a), Generic a) => ToJSON (WithNamedField a) where
    toJSON (WithNamedField a) = gToNamedJson a

instance (GNamedFromJSON (Rep a), Generic a) => FromJSON (WithNamedField a) where
    parseJSON = fmap WithNamedField . gParseNamedJson

gToNamedJson :: (GNamedToJSON (Rep a), Generic a) => a -> Value
gToNamedJson a = Object . HM.fromList $ evalState (gToTupleList $ from a) []

type UsedName = Text

class GNamedToJSON a where
    gToTupleList :: a x -> State [UsedName] [(Text, Value)]

instance (GNamedToJSON f, Datatype d) => GNamedToJSON (M1 D d f) where
    gToTupleList = gToTupleList . unM1

instance (GNamedToJSON f, Constructor c) => GNamedToJSON (M1 C c f) where
    gToTupleList a = do
        used <- get
        put $ used <> ["tag"]
        rest <- gToTupleList $ unM1 a
        pure $ [("tag", String . T.pack $ conName a)] <> rest

instance (JsonFieldName t) => GNamedToJSON (M1 S c (Rec0 t)) where
    gToTupleList a = do
        used <- get
        let fName = fieldName @t
        put $ used <> [fName]
        pure [(actualFieldName used fName, toJSON . unK1 $ unM1 a)]

instance GNamedToJSON U1 where
    gToTupleList U1 = pure []

instance (GNamedToJSON a, GNamedToJSON b) => GNamedToJSON (a :*: b) where
    gToTupleList (a :*: b) = do
        p1 <- gToTupleList a
        p2 <- gToTupleList b
        pure $ p1 <> p2

instance (GNamedToJSON a, GNamedToJSON b) => GNamedToJSON (a :+: b) where
    gToTupleList = \case
        L1 a -> gToTupleList a
        R1 a -> gToTupleList a


actualFieldName :: [UsedName] -> Text -> Text
actualFieldName used fName = fName <> case length (filter (== fName) used) of
    0 -> ""
    i -> "_" <> (T.pack $ show (i + 1))

-----------------------------------------------------------------------------

lookupKey :: Text -> Value -> StateT [UsedName] Parser Value
lookupKey k = \case
    Object o -> do
        used <- get
        put $ used <> [k]
        maybe (fail $ "No key " <> show k) pure $ HM.lookup k o
    _ -> fail $ "Expected " <> show k <> " to be an object."

gParseNamedJson :: (GNamedFromJSON (Rep a), Generic a) => Value -> Parser a
gParseNamedJson v = to <$> evalStateT (gNamedFromJSON v) []

class GNamedFromJSON a where
  gNamedFromJSON :: Value -> StateT [UsedName] Parser (a x)

instance GNamedFromJSON p => GNamedFromJSON (M1 D f p) where
    gNamedFromJSON v = M1 <$> gNamedFromJSON v

instance (Constructor f, GNamedFromJSON p) => GNamedFromJSON (M1 C f p) where
    gNamedFromJSON v = do
        c   <- M1 <$> gNamedFromJSON v
        tag <- lookupKey "tag" v
        case tag of
            String t | T.unpack t == conName c -> pure c
            _ -> fail "Unknown tag"

instance GNamedFromJSON U1 where
    gNamedFromJSON _ = pure U1

instance (GNamedFromJSON a, GNamedFromJSON b) => GNamedFromJSON (a :+: b) where
    gNamedFromJSON vals = L1 <$> gNamedFromJSON vals <|> R1 <$> gNamedFromJSON vals


instance (GNamedFromJSON a, GNamedFromJSON b) => GNamedFromJSON (a :*: b) where
    gNamedFromJSON vals = do
        p1 <- gNamedFromJSON vals
        p2 <- gNamedFromJSON vals
        pure $ p1 :*: p2

instance JsonFieldName t => GNamedFromJSON (M1 S c (Rec0 t)) where
    gNamedFromJSON vals = do
        used <- get
        let fName = fieldName @t
        v <- lookupKey (actualFieldName used fName) vals
        put $ used <> [fName]
        lift $ M1 . K1 <$> (parseJSON @t) v
