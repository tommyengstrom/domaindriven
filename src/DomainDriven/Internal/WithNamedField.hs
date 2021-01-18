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
import           Data.OpenApi            hiding ( put
                                                , get
                                                )
import           Data.Kind                      ( Type )
import           Data.OpenApi.Declare
import           Data.Proxy
import           Control.Lens            hiding ( to
                                                , from
                                                )

newtype WithNamedField a = WithNamedField a

instance (GNamedToJSON (Rep a), Generic a) => ToJSON (WithNamedField a) where
    toJSON (WithNamedField a) = gToNamedJson a

instance (GNamedFromJSON (Rep a), Generic a) => FromJSON (WithNamedField a) where
    parseJSON = fmap WithNamedField . gParseNamedJson

gToNamedJson :: (GNamedToJSON (Rep a), Generic a) => a -> Value
gToNamedJson a = Object . HM.fromList $ evalState (gToTupleList $ from a) []

-----------------------------------------ToSchema-----------------------------------------

data Fancy = Fancy
    { firstField  :: Int
    , secondField :: Maybe String
    }
        | Simple
        {firstField :: Int}
    deriving (Show, Generic, ToJSON, FromJSON, ToSchema)
-- > BL.putStrLn . encode $ toSchema $ Proxy @Fancy
-- {
--    "type" : "object",
--    "oneOf" : [
--       {
--          "properties" : {
--             "tag" : {
--                "type" : "string",
--                "enum" : [
--                   "Fancy"
--                ]
--             },
--             "secondField" : {
--                "type" : "string"
--             },
--             "firstField" : {
--                "type" : "integer",
--                "minimum" : -9223372036854775808,
--                "maximum" : 9223372036854775807
--             }
--          },
--          "type" : "object",
--          "required" : [
--             "firstField",
--             "tag"
--          ]
--       },
--       {
--          "required" : [
--             "firstField",
--             "tag"
--          ],
--          "type" : "object",
--          "properties" : {
--             "tag" : {
--                "type" : "string",
--                "enum" : [
--                   "Simple"
--                ]
--             },
--             "firstField" : {
--                "type" : "integer",
--                "minimum" : -9223372036854775808,
--                "maximum" : 9223372036854775807
--             }
--          }
--       }
--    ]
-- }

data Apa = Apa Int | Bulle String deriving (Show, Generic)
--type instance Rep Apa
--  = D1
--      ('MetaData
--         "Apa" "DomainDriven.Internal.WithNamedField" "main" 'False)
--      (C1
--         ('MetaCons "Apa" 'PrefixI 'False)
--         (S1
--            ('MetaSel
--               'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
--            (Rec0 Int))
--       :+: C1
--             ('MetaCons "Bulle" 'PrefixI 'False)
--             (S1
--                ('MetaSel
--                   'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
--                (Rec0 String)))

data Test = Test
    { firstField  :: Int
    , secondField :: Maybe String
    }
    deriving (Show, Generic, ToJSON, FromJSON, ToSchema)
--type instance Rep Test
--  = D1
--      ('MetaData
--         "Test" "DomainDriven.Internal.WithNamedField" "main" 'False)
--      (C1
--         ('MetaCons "Test" 'PrefixI 'True)
--         (S1
--            ('MetaSel
--               ('Just "firstField")
--               'NoSourceUnpackedness
--               'NoSourceStrictness
--               'DecidedLazy)
--            (Rec0 Int)
--          :*: S1
--                ('MetaSel
--                   ('Just "secondField")
--                   'NoSourceUnpackedness
--                   'NoSourceStrictness
--                   'DecidedLazy)
--                (Rec0 (Maybe String))))

--instance ToSchema Test where
--    declareNamedSchema _ = gDeclareNamedSchema (Proxy @(Rep Test))


-------------------------------------------------
data Proxy3 a b c = Proxy3

class GNamedToSchema (f :: Type -> Type) where
    gDeclareNamedSchema :: Proxy f -> Declare (Definitions Schema) NamedSchema

setSchemaName :: String -> NamedSchema -> NamedSchema
setSchemaName newName (NamedSchema _ s) = NamedSchema (Just $ T.pack newName) s

-- Grab the name of the datatype
instance (Datatype d, GNamedToSchema f) => GNamedToSchema (D1 d f) where
    gDeclareNamedSchema _ = do
        let dtName :: String
            dtName = datatypeName $ Proxy3 @d @f
        NamedSchema _ rest <- gDeclareNamedSchema (Proxy @f)
        pure $ NamedSchema (Just $ T.pack dtName) rest

-- Grab the name of the constructor to use for the `tag` field content.
instance (GNamedToSchema f, Constructor c) => GNamedToSchema (C1 c f) where
    gDeclareNamedSchema _ = do
        NamedSchema _ s <- gDeclareNamedSchema (Proxy @f)
        let constructorName :: Text
            constructorName = T.pack . conName $ Proxy3 @c @f

            tagFieldSchema :: Schema
            tagFieldSchema =
                mempty
                    &   properties
                    <>~ [ ( "tag"
                          , Inline
                          $  mempty
                          &  type_
                          ?~ OpenApiString
                          &  enum_
                          ?~ [String constructorName]
                          )
                        ]
                    &   required
                    <>~ ["tag"]
        pure $ NamedSchema Nothing $ tagFieldSchema <> s

-- Grab the name of the field, but not not set it as required
instance {-# OVERLAPPING #-} (ToSchema f, JsonFieldName f, Selector s)
        => GNamedToSchema (S1 s (Rec0 (Maybe f))) where
    gDeclareNamedSchema _ = do
        pure
            .   NamedSchema Nothing
            $   mempty
            &   properties
            <>~ [(fieldName @(Maybe f), Inline $ toSchema $ Proxy @f)]

-- Grab the name of the field and set it as required
instance {-# OVERLAPPABLE #-} (ToSchema f, JsonFieldName f, Selector s)
        => GNamedToSchema (S1 s (Rec0 f)) where
    gDeclareNamedSchema _ = do
        pure
            .   NamedSchema Nothing
            $   mempty
            &   properties
            <>~ [(fieldName @f, Inline $ toSchema $ Proxy @f)]
            &   required
            <>~ [fieldName @f]

instance GNamedToSchema U1 where
    gDeclareNamedSchema _ = pure $ NamedSchema Nothing mempty

instance (GNamedToSchema f, GNamedToSchema g) => GNamedToSchema (f :*: g) where
    gDeclareNamedSchema _ = do
        NamedSchema _ a <- gDeclareNamedSchema (Proxy @f)
        NamedSchema _ b <- gDeclareNamedSchema (Proxy @g)
        pure (NamedSchema Nothing $ a <> b)


data Simplest = Simplest Int (Maybe String)
    deriving (Show, Generic)

instance ToSchema Simplest where
    declareNamedSchema _ = gDeclareNamedSchema (Proxy @(Rep Simplest))

--class GToSchema (f :: * -> *) where
--  gdeclareNamedSchema :: SchemaOptions -> Proxy f -> Schema -> Declare (Definitions Schema) NamedSchema

------------------------------------------ToJSON------------------------------------------
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

-----------------------------------------FromJSON-----------------------------------------

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
