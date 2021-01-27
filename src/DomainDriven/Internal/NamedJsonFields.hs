{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module DomainDriven.Internal.NamedJsonFields where

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
import           Data.Generics.Product
import           Data.OpenApi            hiding ( put
                                                , get
                                                )
import           Data.Kind                      ( Type )
import           Data.OpenApi.Declare
import           Data.Proxy
import           Control.Lens            hiding ( to
                                                , from
                                                )
import           Data.Text.Lens                 ( packed )

newtype NamedJsonFields a = NamedJsonFields a

instance (GNamedToJSON (Rep a), Generic a) => ToJSON (NamedJsonFields a) where
    toJSON (NamedJsonFields a) = gNamedToJson defaultNamedJsonOptions a

instance (GNamedFromJSON (Rep a), Generic a) => FromJSON (NamedJsonFields a) where
    parseJSON = fmap NamedJsonFields . gNamedParseJson defaultNamedJsonOptions

instance (GNamedToSchema (Rep a), Generic a) => ToSchema (NamedJsonFields a) where
    declareNamedSchema _ = gNamedDeclareNamedSchema defaultNamedJsonOptions (Proxy @a)
        -- evalStateT (gDeclareNamedSchema defaultNamedJsonOptions $ Proxy @(Rep a)) []

gNamedToJson :: (GNamedToJSON (Rep a), Generic a) => NamedJsonOptions -> a -> Value
gNamedToJson opts a = Object . HM.fromList $ evalState (gToTupleList opts $ from a) []

gNamedParseJson
    :: (GNamedFromJSON (Rep a), Generic a) => NamedJsonOptions -> Value -> Parser a
gNamedParseJson opts v = to <$> evalStateT (gNamedFromJSON opts v) []

gNamedDeclareNamedSchema
    :: forall a
     . (GNamedToSchema (Rep a), Generic a)
    => NamedJsonOptions
    -> Proxy a
    -> Declare (Definitions Schema) NamedSchema
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
-----------------------------------------ToSchema-----------------------------------------
data Proxy3 a b c = Proxy3

class GNamedToSchema (f :: Type -> Type) where
    gDeclareNamedSchema :: NamedJsonOptions
                        -> Proxy f
                        -> StateT [UsedName] (Declare (Definitions Schema)) NamedSchema

-- Grab the name of the datatype
instance (Datatype d, GNamedToSchema f) => GNamedToSchema (D1 d f) where
    gDeclareNamedSchema opts _ = do
        let dtName :: String
            dtName = opts ^. field @"datatypeNameModifier" $ datatypeName (Proxy3 @d @f)
        NamedSchema _ rest <- gDeclareNamedSchema opts (Proxy @f)
        pure $ NamedSchema (Just $ T.pack dtName) rest

-- Grab the name of the constructor to use for the `tag` field content.
instance (GNamedToSchema f, Constructor c) => GNamedToSchema (C1 c f) where
    gDeclareNamedSchema opts _ = do
        NamedSchema _ s <- gDeclareNamedSchema opts (Proxy @f)
        if opts ^. field @"skipTagField"
            then pure $ NamedSchema Nothing s
            else do
                let tagName = opts ^. field @"tagFieldName" . packed
                state $ \ss -> ((), tagName : ss)
                let constructorName :: Text
                    constructorName =
                        T.pack
                            . (opts ^. field @"constructorTagModifier")
                            . conName
                            $ Proxy3 @c @f

                    tagFieldSchema :: Schema
                    tagFieldSchema =
                        mempty
                            &   properties
                            <>~ [ ( tagName
                                  , Inline
                                  $  mempty
                                  &  type_
                                  ?~ OpenApiString
                                  &  enum_
                                  ?~ [String constructorName]
                                  )
                                ]
                            &   required
                            <>~ [tagName]
                pure $ NamedSchema Nothing $ tagFieldSchema <> s

-- Grab the name of the field, but not not set it as required
instance {-# OVERLAPPING #-} (ToSchema f, JsonFieldName f, Selector s)
        => GNamedToSchema (S1 s (Rec0 (Maybe f))) where
    gDeclareNamedSchema _opts _ = do
        let fName = fieldName @(Maybe f)
        usedNames <- state (\used -> (used, fName : used))
        pure
            .   NamedSchema Nothing
            $   mempty
            &   properties
            <>~ [(actualFieldName usedNames fName, Inline $ toSchema $ Proxy @f)]

-- Grab the name of the field and set it as required
instance {-# OVERLAPPABLE #-} (ToSchema f, JsonFieldName f, Selector s)
        => GNamedToSchema (S1 s (Rec0 f)) where
    gDeclareNamedSchema _opts _ = do
        let fName = fieldName @f
        usedNames <- state (\used -> (used, fName : used))
        pure
            .   NamedSchema Nothing
            $   mempty
            &   properties
            <>~ [(actualFieldName usedNames fName, Inline $ toSchema $ Proxy @f)]
            &   required
            <>~ [actualFieldName usedNames fName]

instance GNamedToSchema U1 where
    gDeclareNamedSchema _opts _ = pure $ NamedSchema Nothing mempty

instance (GNamedToSchema f, GNamedToSchema g) => GNamedToSchema (f :*: g) where
    gDeclareNamedSchema opts _ = do
        NamedSchema _ a <- gDeclareNamedSchema opts (Proxy @f)
        NamedSchema _ b <- gDeclareNamedSchema opts (Proxy @g)
        pure (NamedSchema Nothing $ a <> b)

instance (GNamedToSchema f, GNamedToSchema g) => GNamedToSchema (f :+: g) where
    gDeclareNamedSchema opts _ = do
        -- Sum types do not share fields, thus we do not need to adjust the names
        NamedSchema _ a <- lift $ evalStateT (gDeclareNamedSchema opts (Proxy @f)) []
        NamedSchema _ b <- lift $ evalStateT (gDeclareNamedSchema opts (Proxy @g)) []
        let unwrapOneOf :: Schema -> [Referenced Schema]
            unwrapOneOf x = (maybe [Inline x] id $ x ^. oneOf)
        pure $ NamedSchema Nothing $ mempty & oneOf .~ Just
            (unwrapOneOf a <> unwrapOneOf b)


------------------------------------------ToJSON------------------------------------------
type UsedName = Text

class GNamedToJSON a where
    gToTupleList :: NamedJsonOptions -> a x -> State [UsedName] [(Text, Value)]

instance (GNamedToJSON f, Datatype d) => GNamedToJSON (M1 D d f) where
    gToTupleList opts = gToTupleList opts . unM1

instance (GNamedToJSON f, Constructor c) => GNamedToJSON (M1 C c f) where
    gToTupleList opts a = do
        tag <- if opts ^. field @"skipTagField"
            then pure []
            else do
                usedNames <- get
                put $ usedNames <> [opts ^. field @"tagFieldName" . packed]
                pure
                    [ ( (opts ^. field @"tagFieldName" . packed)
                      , String
                      . T.pack
                      . (opts ^. field @"constructorTagModifier")
                      $ conName a
                      )
                    ]
        rest <- gToTupleList opts $ unM1 a
        pure $ tag <> rest

instance (ToJSON t, JsonFieldName t) => GNamedToJSON (M1 S c (Rec0 t)) where
    gToTupleList _opts a = do
        usedNames <- get
        let fName = fieldName @t
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


actualFieldName :: [UsedName] -> Text -> Text
actualFieldName usedNames fName = fName <> case length (filter (== fName) usedNames) of
    0 -> ""
    i -> "_" <> (T.pack $ show (i + 1))

-----------------------------------------FromJSON-----------------------------------------

lookupKey :: Text -> Value -> StateT [UsedName] Parser Value
lookupKey k = \case
    Object o -> do
        usedNames <- get
        put $ usedNames <> [k]
        maybe (fail $ "No key " <> show k) pure $ HM.lookup k o
    _ -> fail $ "Expected " <> show k <> " to be an object."

class GNamedFromJSON a where
  gNamedFromJSON :: NamedJsonOptions -> Value -> StateT [UsedName] Parser (a x)

instance GNamedFromJSON p => GNamedFromJSON (M1 D f p) where
    gNamedFromJSON opts v = M1 <$> gNamedFromJSON opts v

instance (Constructor f, GNamedFromJSON p) => GNamedFromJSON (M1 C f p) where
    gNamedFromJSON opts v = if opts ^. field @"skipTagField"
        then M1 <$> gNamedFromJSON opts v
        else do
            tag <- lookupKey (opts ^. field @"tagFieldName" . packed) v
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

instance (FromJSON t, JsonFieldName t) => GNamedFromJSON (M1 S c (Rec0 t)) where
    gNamedFromJSON _opts vals = do
        usedNames <- get
        let fName = fieldName @t
        v <- lookupKey (actualFieldName usedNames fName) vals
        put $ usedNames <> [fName]
        lift $ M1 . K1 <$> (parseJSON @t) v
