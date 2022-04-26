{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module DomainDriven.Internal.NamedJsonFields where

import           Control.Applicative
import           Control.Lens            hiding ( from
                                                , to
                                                )
import           Control.Monad.State
import           Data.Aeson
import qualified Data.Aeson.Key                               as Key
import qualified Data.Aeson.KeyMap                            as KM
import           Data.Aeson.Types
import           Data.Generics.Product
import           Data.Kind                      ( Type )
import qualified Data.OpenApi                                 as O
import           Data.OpenApi.Declare
import           Data.Proxy
import           Data.Text                      ( Text )
import qualified Data.Text                                    as T
import           Data.Text.Lens                 ( packed )
import           Data.Typeable
import           DomainDriven.Internal.HasFieldName
import           GHC.Generics
import           Prelude

newtype NamedJsonFields a = NamedJsonFields a

instance (GNamedToJSON (Rep a), Generic a) => ToJSON (NamedJsonFields a) where
    toJSON (NamedJsonFields a) = gNamedToJson defaultNamedJsonOptions a

instance (GNamedFromJSON (Rep a), Generic a) => FromJSON (NamedJsonFields a) where
    parseJSON = fmap NamedJsonFields . gNamedParseJson defaultNamedJsonOptions

instance (Typeable a, GNamedToSchema (Rep a), Generic a) => O.ToSchema (NamedJsonFields a) where
    declareNamedSchema _ = gNamedDeclareNamedSchema defaultNamedJsonOptions (Proxy @a)
        -- evalStateT (gDeclareNamedSchema defaultNamedJsonOptions $ Proxy @(Rep a)) []

gNamedToJson :: (GNamedToJSON (Rep a), Generic a) => NamedJsonOptions -> a -> Value
gNamedToJson opts a = Object . KM.fromList $ evalState (gToTupleList opts $ from a) []

gNamedParseJson
    :: (GNamedFromJSON (Rep a), Generic a) => NamedJsonOptions -> Value -> Parser a
gNamedParseJson opts v = to <$> evalStateT (gNamedFromJSON opts v) []

gNamedDeclareNamedSchema
    :: forall a
     . (GNamedToSchema (Rep a), Generic a)
    => NamedJsonOptions
    -> Proxy a
    -> Declare (O.Definitions O.Schema) O.NamedSchema
gNamedDeclareNamedSchema opts _ =
    evalStateT (gDeclareNamedSchema opts (Proxy @(Rep a))) []
-----------------------------------------Options------------------------------------------

data NamedJsonOptions = NamedJsonOptions
    { constructorTagModifier :: String -> String
    , tagFieldName           :: String
    , skipTagField           :: Bool
    , datatypeNameModifier   :: String -> String
    }
    deriving Generic

defaultNamedJsonOptions :: NamedJsonOptions
defaultNamedJsonOptions = NamedJsonOptions { constructorTagModifier = id
                                           , tagFieldName           = "tag"
                                           , skipTagField           = False
                                           , datatypeNameModifier   = id
                                           }
-----------------------------------------O.ToSchema-----------------------------------------
data Proxy3 a b c = Proxy3

class GNamedToSchema (f :: Type -> Type) where
    gDeclareNamedSchema :: NamedJsonOptions
                        -> Proxy f
                        -> StateT [UsedName] (Declare (O.Definitions O.Schema)) O.NamedSchema

-- Grab the name of the datatype
instance (Datatype d, GNamedToSchema f) => GNamedToSchema (D1 d f) where
    gDeclareNamedSchema opts _ = do
        let dtName :: String
            dtName = opts ^. field @"datatypeNameModifier" $ datatypeName (Proxy3 @d @f)
        O.NamedSchema _ rest <- gDeclareNamedSchema opts (Proxy @f)
        pure $ O.NamedSchema (Just $ T.pack dtName) rest

-- Grab the name of the constructor to use for the `tag` field content.
instance (GNamedToSchema f, Constructor c) => GNamedToSchema (C1 c f) where
    gDeclareNamedSchema opts _ = do
        O.NamedSchema _ s <- gDeclareNamedSchema opts (Proxy @f)
        if opts ^. field @"skipTagField"
            then pure $ O.NamedSchema Nothing s
            else do
                let tagName :: Key
                    tagName = Key.fromText $ opts ^. field @"tagFieldName" . packed
                state $ \ss -> ((), tagName : ss)
                let constructorName :: Text
                    constructorName =
                        T.pack
                            . (opts ^. field @"constructorTagModifier")
                            . conName
                            $ Proxy3 @c @f

                    tagFieldSchema :: O.Schema
                    tagFieldSchema =
                        mempty
                            &   O.properties
                            <>~ [ ( Key.toText tagName
                                  , O.Inline
                                  $  mempty
                                  &  O.type_
                                  ?~ O.OpenApiString
                                  &  O.enum_
                                  ?~ [String constructorName]
                                  )
                                ]
                            &   O.required
                            <>~ [Key.toText tagName]
                pure $ O.NamedSchema Nothing $ tagFieldSchema <> s

-- Grab the name of the field, but not not set it as O.required
instance {-# OVERLAPPING #-} (O.ToSchema f, HasFieldName f, Selector s)
        => GNamedToSchema (S1 s (Rec0 (Maybe f))) where
    gDeclareNamedSchema _opts _ = do
        let fName = Key.fromText $ fieldName @(Maybe f)
        usedNames <- state (\used -> (used, fName : used))
        _         <- lift $ O.declareSchemaRef $ Proxy @f
        pure
            .   O.NamedSchema Nothing
            $   mempty
            &   O.properties
            <>~ [ ( Key.toText $ actualFieldName usedNames fName
                  , O.Inline $ O.toSchema $ Proxy @f
                  )
                ]

-- Grab the name of the field and set it as O.required
instance {-# OVERLAPPABLE #-} (O.ToSchema f, HasFieldName f, Selector s)
        => GNamedToSchema (S1 s (Rec0 f)) where
    gDeclareNamedSchema _opts _ = do
        let fName = Key.fromText $ fieldName @f
        usedNames <- state (\used -> (used, fName : used))
        _         <- lift $ O.declareSchemaRef $ Proxy @f
        pure
            .   O.NamedSchema Nothing
            $   mempty
            &   O.properties
            <>~ [ ( Key.toText $ actualFieldName usedNames fName
                  , O.Inline $ O.toSchema $ Proxy @f
                  )
                ]
            &   O.required
            <>~ [Key.toText $ actualFieldName usedNames fName]

instance GNamedToSchema U1 where
    gDeclareNamedSchema _opts _ = pure $ O.NamedSchema Nothing mempty

instance (GNamedToSchema f, GNamedToSchema g) => GNamedToSchema (f :*: g) where
    gDeclareNamedSchema opts _ = do
        O.NamedSchema _ a <- gDeclareNamedSchema opts (Proxy @f)
        O.NamedSchema _ b <- gDeclareNamedSchema opts (Proxy @g)
        pure (O.NamedSchema Nothing $ a <> b)

instance (GNamedToSchema f, GNamedToSchema g) => GNamedToSchema (f :+: g) where
    gDeclareNamedSchema opts _ = do
        -- Sum types do not share fields, thus we do not need to adjust the names
        O.NamedSchema _ a <- lift $ evalStateT (gDeclareNamedSchema opts (Proxy @f)) []
        O.NamedSchema _ b <- lift $ evalStateT (gDeclareNamedSchema opts (Proxy @g)) []
        let unwrapOneOf :: O.Schema -> [O.Referenced O.Schema]
            unwrapOneOf x = (maybe [O.Inline x] id $ x ^. O.oneOf)
        pure $ O.NamedSchema Nothing $ mempty & O.oneOf .~ Just
            (unwrapOneOf a <> unwrapOneOf b)


------------------------------------------ToJSON------------------------------------------
type UsedName = Key

class GNamedToJSON a where
    gToTupleList :: NamedJsonOptions -> a x -> State [UsedName] [(Key, Value)]

instance (GNamedToJSON f, Datatype d) => GNamedToJSON (M1 D d f) where
    gToTupleList opts = gToTupleList opts . unM1

instance (GNamedToJSON f, Constructor c) => GNamedToJSON (M1 C c f) where
    gToTupleList opts a = do
        tag <- if opts ^. field @"skipTagField"
            then pure []
            else do
                usedNames <- get
                put $ usedNames <> [Key.fromText $ opts ^. field @"tagFieldName" . packed]
                pure
                    [ ( (Key.fromText $ opts ^. field @"tagFieldName" . packed)
                      , String
                      . T.pack
                      . (opts ^. field @"constructorTagModifier")
                      $ conName a
                      )
                    ]
        rest <- gToTupleList opts $ unM1 a
        pure $ tag <> rest

instance (ToJSON t, HasFieldName t) => GNamedToJSON (M1 S c (Rec0 t)) where
    gToTupleList _opts a = do
        usedNames <- get
        let fName = Key.fromText $ fieldName @t
        put $ usedNames <> [fName]
        pure [(actualFieldName usedNames fName, toJSON . unK1 $ unM1 a)]

instance GNamedToJSON U1 where
    gToTupleList _opts U1 = pure []

instance (GNamedToJSON a, GNamedToJSON b) => GNamedToJSON (a :*: b) where
    gToTupleList opts (a :*: b) = do
        p1 <- gToTupleList opts a
        p2 <- gToTupleList opts b
        pure $ p1 <> p2

instance (GNamedToJSON a, GNamedToJSON b) => GNamedToJSON (a :+: b) where
    gToTupleList opts = \case
        L1 a -> gToTupleList opts a
        R1 a -> gToTupleList opts a


actualFieldName :: [UsedName] -> Key -> Key
actualFieldName usedNames fName = fName <> case length (filter (== fName) usedNames) of
    0 -> Key.fromText ""
    i -> Key.fromText $ "_" <> (T.pack $ show (i + 1))

-----------------------------------------FromJSON-----------------------------------------

lookupKey :: Key -> Value -> StateT [UsedName] Parser Value
lookupKey k = \case
    Object o -> do
        usedNames <- get
        put $ usedNames <> [k]
        maybe (fail $ "No key " <> show k) pure $ KM.lookup k o
    _ -> fail $ "Expected " <> show k <> " to be an object."

class GNamedFromJSON a where
  gNamedFromJSON :: NamedJsonOptions -> Value -> StateT [UsedName] Parser (a x)

instance GNamedFromJSON p => GNamedFromJSON (M1 D f p) where
    gNamedFromJSON opts v = M1 <$> gNamedFromJSON opts v

instance (Constructor f, GNamedFromJSON p) => GNamedFromJSON (M1 C f p) where
    gNamedFromJSON opts v = if opts ^. field @"skipTagField"
        then M1 <$> gNamedFromJSON opts v
        else do
            tag <- lookupKey (Key.fromText $ opts ^. field @"tagFieldName" . packed) v
            c   <- M1 <$> gNamedFromJSON opts v
            let constructorName =
                    T.pack . (opts ^. field @"constructorTagModifier") $ conName c
            case tag of
                String t | t == constructorName -> pure c
                _                               -> fail "Unknown tag"

instance GNamedFromJSON U1 where
    gNamedFromJSON _opts _ = pure U1

instance (GNamedFromJSON a, GNamedFromJSON b) => GNamedFromJSON (a :+: b) where
    gNamedFromJSON opts vals =
        L1 <$> gNamedFromJSON opts vals <|> R1 <$> gNamedFromJSON opts vals


instance (GNamedFromJSON a, GNamedFromJSON b) => GNamedFromJSON (a :*: b) where
    gNamedFromJSON opts vals = do
        p1 <- gNamedFromJSON opts vals
        p2 <- gNamedFromJSON opts vals
        pure $ p1 :*: p2

instance (FromJSON t, HasFieldName t) => GNamedFromJSON (M1 S c (Rec0 t)) where
    gNamedFromJSON _opts vals = do
        usedNames <- get
        let fName = Key.fromText $ fieldName @t
        v <- lookupKey (actualFieldName usedNames fName) vals
        put $ usedNames <> [fName]
        lift $ M1 . K1 <$> (parseJSON @t) v
