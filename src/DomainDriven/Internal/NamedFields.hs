{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}

module DomainDriven.Internal.NamedFields
    ( NF1 (..)
    , NF2 (..)
    , NF3 (..)
    , NF4 (..)
    , NF5 (..)
    , NF6 (..)
    , NF7 (..)
    , NF8 (..)
    , NF9 (..)
    ) where

import           Data.Aeson
import           Data.OpenApi
import           Prelude
import           GHC.Generics                   ( Generic )
import           Data.Proxy
import           GHC.TypeLits
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as Key
import qualified Data.Text as T

data NF1 (name :: Symbol) (f1 :: Symbol) ty = NF1 ty
    deriving (Show, Generic)

data NF2 (name :: Symbol) (f1 :: Symbol) a1 (f2 :: Symbol) a2 = NF2 a1 a2
    deriving (Show, Generic)

data NF3 (name :: Symbol) (f1 :: Symbol) a1 (f2 :: Symbol) a2 (f3 :: Symbol) a3 = NF3 a1 a2 a3
    deriving (Show, Generic)

data NF4 (name :: Symbol) (f1 :: Symbol) a1 (f2 :: Symbol) a2 (f3 :: Symbol) a3 (f4 :: Symbol) a4 = NF4 a1 a2 a3 a4
    deriving (Show, Generic)

data NF5 (name :: Symbol) (f1 :: Symbol) a1 (f2 :: Symbol) a2 (f3 :: Symbol) a3 (f4 :: Symbol) a4 (f5 :: Symbol) a5 = NF5 a1 a2 a3 a4 a5
    deriving (Show, Generic)

data NF6 (name :: Symbol) (f1 :: Symbol) a1 (f2 :: Symbol) a2 (f3 :: Symbol) a3 (f4 :: Symbol) a4 (f5 :: Symbol) a5 (f6 :: Symbol) a6 = NF6 a1 a2 a3 a4 a5 a6
    deriving (Show, Generic)

data NF7 (name :: Symbol) (f1 :: Symbol) a1 (f2 :: Symbol) a2 (f3 :: Symbol) a3 (f4 :: Symbol) a4 (f5 :: Symbol) a5 (f6 :: Symbol) a6 (f7 :: Symbol) a7 = NF7 a1 a2 a3 a4 a5 a6 a7
    deriving (Show, Generic)

data NF8 (name :: Symbol) (f1 :: Symbol) a1 (f2 :: Symbol) a2 (f3 :: Symbol) a3 (f4 :: Symbol) a4 (f5 :: Symbol) a5 (f6 :: Symbol) a6 (f7 :: Symbol) a7 (f8 :: Symbol) a8 = NF8 a1 a2 a3 a4 a5 a6 a7 a8
    deriving (Show, Generic)

data NF9 (name :: Symbol) (f1 :: Symbol) a1 (f2 :: Symbol) a2 (f3 :: Symbol) a3 (f4 :: Symbol) a4 (f5 :: Symbol) a5 (f6 :: Symbol) a6 (f7 :: Symbol) a7 (f8 :: Symbol) a8 (f9 :: Symbol) a9 = NF9 a1 a2 a3 a4 a5 a6 a7 a8 a9
    deriving (Show, Generic)

symbolKey :: forall n. KnownSymbol n => Key
symbolKey = Key.fromString . symbolVal $ Proxy @n



instance (KnownSymbol f1, ToJSON a1) => ToJSON (NF1 name f1 a1) where
    toJSON (NF1 a1) = Object $ KM.fromList
        [(symbolKey @f1, toJSON a1)
        ]

instance (KnownSymbol f1, ToJSON a1
         ,KnownSymbol f2, ToJSON a2
          )
    => ToJSON (NF2 name f1 a1 f2 a2) where
    toJSON (NF2 a1 a2) = Object $ KM.fromList
        [(symbolKey @f1, toJSON a1)
        ,(symbolKey @f2, toJSON a2)
        ]

instance (KnownSymbol f1, ToJSON a1
         ,KnownSymbol f2, ToJSON a2
         ,KnownSymbol f3, ToJSON a3
          )
    => ToJSON (NF3 name f1 a1 f2 a2 f3 a3) where
    toJSON (NF3 a1 a2 a3) = Object $ KM.fromList
        [(symbolKey @f1, toJSON a1)
        ,(symbolKey @f2, toJSON a2)
        ,(symbolKey @f3, toJSON a3)
        ]
instance (KnownSymbol f1, ToJSON a1
         ,KnownSymbol f2, ToJSON a2
         ,KnownSymbol f3, ToJSON a3
         ,KnownSymbol f4, ToJSON a4
          )
    => ToJSON (NF4 name f1 a1 f2 a2 f3 a3 f4 a4) where
    toJSON (NF4 a1 a2 a3 a4) = Object $ KM.fromList
        [(symbolKey @f1, toJSON a1)
        ,(symbolKey @f2, toJSON a2)
        ,(symbolKey @f3, toJSON a3)
        ,(symbolKey @f4, toJSON a4)
        ]
instance (KnownSymbol f1, ToJSON a1
         ,KnownSymbol f2, ToJSON a2
         ,KnownSymbol f3, ToJSON a3
         ,KnownSymbol f4, ToJSON a4
         ,KnownSymbol f5, ToJSON a5
          )
    => ToJSON (NF5 name f1 a1 f2 a2 f3 a3 f4 a4 f5 a5) where
    toJSON (NF5 a1 a2 a3 a4 a5) = Object $ KM.fromList
        [(symbolKey @f1, toJSON a1)
        ,(symbolKey @f2, toJSON a2)
        ,(symbolKey @f3, toJSON a3)
        ,(symbolKey @f4, toJSON a4)
        ,(symbolKey @f5, toJSON a5)
        ]
instance (KnownSymbol f1, ToJSON a1
         ,KnownSymbol f2, ToJSON a2
         ,KnownSymbol f3, ToJSON a3
         ,KnownSymbol f4, ToJSON a4
         ,KnownSymbol f5, ToJSON a5
         ,KnownSymbol f6, ToJSON a6
          )
    => ToJSON (NF6 name f1 a1 f2 a2 f3 a3 f4 a4 f5 a5 f6 a6) where
    toJSON (NF6 a1 a2 a3 a4 a5 a6) = Object $ KM.fromList
        [(symbolKey @f1, toJSON a1)
        ,(symbolKey @f2, toJSON a2)
        ,(symbolKey @f3, toJSON a3)
        ,(symbolKey @f4, toJSON a4)
        ,(symbolKey @f5, toJSON a5)
        ,(symbolKey @f6, toJSON a6)
        ]
instance (KnownSymbol f1, ToJSON a1
         ,KnownSymbol f2, ToJSON a2
         ,KnownSymbol f3, ToJSON a3
         ,KnownSymbol f4, ToJSON a4
         ,KnownSymbol f5, ToJSON a5
         ,KnownSymbol f6, ToJSON a6
         ,KnownSymbol f7, ToJSON a7
          )
    => ToJSON (NF7 name f1 a1 f2 a2 f3 a3 f4 a4 f5 a5 f6 a6 f7 a7) where
    toJSON (NF7 a1 a2 a3 a4 a5 a6 a7) = Object $ KM.fromList
        [(symbolKey @f1, toJSON a1)
        ,(symbolKey @f2, toJSON a2)
        ,(symbolKey @f3, toJSON a3)
        ,(symbolKey @f4, toJSON a4)
        ,(symbolKey @f5, toJSON a5)
        ,(symbolKey @f6, toJSON a6)
        ,(symbolKey @f7, toJSON a7)
        ]
instance (KnownSymbol f1, ToJSON a1
         ,KnownSymbol f2, ToJSON a2
         ,KnownSymbol f3, ToJSON a3
         ,KnownSymbol f4, ToJSON a4
         ,KnownSymbol f5, ToJSON a5
         ,KnownSymbol f6, ToJSON a6
         ,KnownSymbol f7, ToJSON a7
         ,KnownSymbol f8, ToJSON a8
          )
    => ToJSON (NF8 name f1 a1 f2 a2 f3 a3 f4 a4 f5 a5 f6 a6 f7 a7 f8 a8) where
    toJSON (NF8 a1 a2 a3 a4 a5 a6 a7 a8) = Object $ KM.fromList
        [(symbolKey @f1, toJSON a1)
        ,(symbolKey @f2, toJSON a2)
        ,(symbolKey @f3, toJSON a3)
        ,(symbolKey @f4, toJSON a4)
        ,(symbolKey @f5, toJSON a5)
        ,(symbolKey @f6, toJSON a6)
        ,(symbolKey @f7, toJSON a7)
        ,(symbolKey @f8, toJSON a8)
        ]
instance (KnownSymbol f1, ToJSON a1
         ,KnownSymbol f2, ToJSON a2
         ,KnownSymbol f3, ToJSON a3
         ,KnownSymbol f4, ToJSON a4
         ,KnownSymbol f5, ToJSON a5
         ,KnownSymbol f6, ToJSON a6
         ,KnownSymbol f7, ToJSON a7
         ,KnownSymbol f8, ToJSON a8
         ,KnownSymbol f9, ToJSON a9
          )
    => ToJSON (NF9 name f1 a1 f2 a2 f3 a3 f4 a4 f5 a5 f6 a6 f7 a7 f8 a8 f9 a9) where
    toJSON (NF9  a1 a2 a3 a4 a5 a6 a7 a8 a9) = Object $ KM.fromList
        [(symbolKey @f1, toJSON a1)
        ,(symbolKey @f2, toJSON a2)
        ,(symbolKey @f3, toJSON a3)
        ,(symbolKey @f4, toJSON a4)
        ,(symbolKey @f5, toJSON a5)
        ,(symbolKey @f6, toJSON a6)
        ,(symbolKey @f7, toJSON a7)
        ,(symbolKey @f8, toJSON a8)
        ,(symbolKey @f9, toJSON a9)
        ]

instance (KnownSymbol name, KnownSymbol f1, FromJSON a1)
    => FromJSON (NF1 name f1 a1) where
    parseJSON = withObject (symbolVal $ Proxy @name) $ \o -> do
        a1 <- o .: symbolKey @f1
        pure $ NF1 a1


instance ( KnownSymbol name
         , KnownSymbol f1, FromJSON a1
         , KnownSymbol f2, FromJSON a2
         )
    => FromJSON (NF2 name f1 a1 f2 a2) where
    parseJSON = withObject (symbolVal $ Proxy @name) $ \o -> do
        a1 <- o .: symbolKey @f1
        a2 <- o .: symbolKey @f2
        pure $ NF2 a1 a2

instance ( KnownSymbol name
         , KnownSymbol f1, FromJSON a1
         , KnownSymbol f2, FromJSON a2
         , KnownSymbol f3, FromJSON a3)
    => FromJSON (NF3 name f1 a1 f2 a2 f3 a3) where
    parseJSON = withObject (symbolVal $ Proxy @name) $ \o -> do
        a1 <- o .: symbolKey @f1
        a2 <- o .: symbolKey @f2
        a3 <- o .: symbolKey @f3
        pure $ NF3 a1 a2 a3

instance ( KnownSymbol name
         , KnownSymbol f1, FromJSON a1
         , KnownSymbol f2, FromJSON a2
         , KnownSymbol f3, FromJSON a3
         , KnownSymbol f4, FromJSON a4)
    => FromJSON (NF4 name f1 a1 f2 a2 f3 a3 f4 a4) where
    parseJSON = withObject (symbolVal $ Proxy @name) $ \o -> do
        a1 <- o .: symbolKey @f1
        a2 <- o .: symbolKey @f2
        a3 <- o .: symbolKey @f3
        a4 <- o .: symbolKey @f4
        pure $ NF4 a1 a2 a3 a4

instance ( KnownSymbol name
         , KnownSymbol f1, FromJSON a1
         , KnownSymbol f2, FromJSON a2
         , KnownSymbol f3, FromJSON a3
         , KnownSymbol f4, FromJSON a4
         , KnownSymbol f5, FromJSON a5)
    => FromJSON (NF5 name f1 a1 f2 a2 f3 a3 f4 a4 f5 a5) where
    parseJSON = withObject (symbolVal $ Proxy @name) $ \o -> do
        a1 <- o .: symbolKey @f1
        a2 <- o .: symbolKey @f2
        a3 <- o .: symbolKey @f3
        a4 <- o .: symbolKey @f4
        a5 <- o .: symbolKey @f5
        pure $ NF5 a1 a2 a3 a4 a5

instance ( KnownSymbol name
         , KnownSymbol f1, FromJSON a1
         , KnownSymbol f2, FromJSON a2
         , KnownSymbol f3, FromJSON a3
         , KnownSymbol f4, FromJSON a4
         , KnownSymbol f5, FromJSON a5
         , KnownSymbol f6, FromJSON a6)
    => FromJSON (NF6 name f1 a1 f2 a2 f3 a3 f4 a4 f5 a5 f6 a6) where
    parseJSON = withObject (symbolVal $ Proxy @name) $ \o -> do
        a1 <- o .: symbolKey @f1
        a2 <- o .: symbolKey @f2
        a3 <- o .: symbolKey @f3
        a4 <- o .: symbolKey @f4
        a5 <- o .: symbolKey @f5
        a6 <- o .: symbolKey @f6
        pure $ NF6 a1 a2 a3 a4 a5 a6

instance ( KnownSymbol name
         , KnownSymbol f1, FromJSON a1
         , KnownSymbol f2, FromJSON a2
         , KnownSymbol f3, FromJSON a3
         , KnownSymbol f4, FromJSON a4
         , KnownSymbol f5, FromJSON a5
         , KnownSymbol f6, FromJSON a6
         , KnownSymbol f7, FromJSON a7)
    => FromJSON (NF7 name f1 a1 f2 a2 f3 a3 f4 a4 f5 a5 f6 a6 f7 a7) where
    parseJSON = withObject (symbolVal $ Proxy @name) $ \o -> do
        a1 <- o .: symbolKey @f1
        a2 <- o .: symbolKey @f2
        a3 <- o .: symbolKey @f3
        a4 <- o .: symbolKey @f4
        a5 <- o .: symbolKey @f5
        a6 <- o .: symbolKey @f6
        a7 <- o .: symbolKey @f7
        pure $ NF7 a1 a2 a3 a4 a5 a6 a7

instance ( KnownSymbol name
         , KnownSymbol f1, FromJSON a1
         , KnownSymbol f2, FromJSON a2
         , KnownSymbol f3, FromJSON a3
         , KnownSymbol f4, FromJSON a4
         , KnownSymbol f5, FromJSON a5
         , KnownSymbol f6, FromJSON a6
         , KnownSymbol f7, FromJSON a7
         , KnownSymbol f8, FromJSON a8)
    => FromJSON (NF8 name f1 a1 f2 a2 f3 a3 f4 a4 f5 a5 f6 a6 f7 a7 f8 a8) where
    parseJSON = withObject (symbolVal $ Proxy @name) $ \o -> do
        a1 <- o .: symbolKey @f1
        a2 <- o .: symbolKey @f2
        a3 <- o .: symbolKey @f3
        a4 <- o .: symbolKey @f4
        a5 <- o .: symbolKey @f5
        a6 <- o .: symbolKey @f6
        a7 <- o .: symbolKey @f7
        a8 <- o .: symbolKey @f8
        pure $ NF8 a1 a2 a3 a4 a5 a6 a7 a8

instance ( KnownSymbol name
         , KnownSymbol f1, FromJSON a1
         , KnownSymbol f2, FromJSON a2
         , KnownSymbol f3, FromJSON a3
         , KnownSymbol f4, FromJSON a4
         , KnownSymbol f5, FromJSON a5
         , KnownSymbol f6, FromJSON a6
         , KnownSymbol f7, FromJSON a7
         , KnownSymbol f8, FromJSON a8
         , KnownSymbol f9, FromJSON a9)
    => FromJSON (NF9 name f1 a1 f2 a2 f3 a3 f4 a4 f5 a5 f6 a6 f7 a7 f8 a8 f9 a9) where
    parseJSON = withObject (symbolVal $ Proxy @name) $ \o -> do
        a1 <- o .: symbolKey @f1
        a2 <- o .: symbolKey @f2
        a3 <- o .: symbolKey @f3
        a4 <- o .: symbolKey @f4
        a5 <- o .: symbolKey @f5
        a6 <- o .: symbolKey @f6
        a7 <- o .: symbolKey @f7
        a8 <- o .: symbolKey @f8
        a9 <- o .: symbolKey @f9
        pure $ NF9 a1 a2 a3 a4 a5 a6 a7 a8 a9



instance ( KnownSymbol name
         , KnownSymbol f1
         , ToSchema a1
         )
    =>  ToSchema (NF1 name f1 a1) where
        declareNamedSchema _ = do
            f1Dec <- declareSchema $ Proxy @a1
            let f1 = T.pack . symbolVal $ Proxy @f1
            -- Not sure we should be inlining the schema. Let
            let nfSchema :: Schema
                nfSchema = mempty
                   { _schemaRequired = [ f1]
                   , _schemaProperties = [(f1, Inline f1Dec)]
                   }
            pure NamedSchema
                { _namedSchemaName = Just . T.pack . symbolVal $ Proxy @name
                , _namedSchemaSchema = nfSchema
                }

instance ( KnownSymbol name
         , KnownSymbol f1
         , ToSchema a1
         , KnownSymbol f2
         , ToSchema a2
         )
    =>  ToSchema (NF2 name f1 a1 f2 a2) where
        declareNamedSchema _ = do
            f1Dec <- declareSchema $ Proxy @a1
            f2Dec <- declareSchema $ Proxy @a2
            let f1 = T.pack . symbolVal $ Proxy @f1
            let f2 = T.pack . symbolVal $ Proxy @f2
            -- Not sure we should be inlining the schema. Let
            let nfSchema :: Schema
                nfSchema = mempty
                   { _schemaRequired = [ f1, f2]
                   , _schemaProperties =
                    [(f1, Inline f1Dec)
                    ,(f2, Inline f2Dec)
                    ]
                   }
            pure NamedSchema
                { _namedSchemaName = Just . T.pack . symbolVal $ Proxy @name
                , _namedSchemaSchema = nfSchema
                }


instance ( KnownSymbol name
         , KnownSymbol f1
         , ToSchema a1
         , KnownSymbol f2
         , ToSchema a2
         , KnownSymbol f3
         , ToSchema a3
         )
    =>  ToSchema (NF3 name f1 a1 f2 a2 f3 a3) where
        declareNamedSchema _ = do
            f1Dec <- declareSchema $ Proxy @a1
            f2Dec <- declareSchema $ Proxy @a2
            f3Dec <- declareSchema $ Proxy @a3
            let f1 = T.pack . symbolVal $ Proxy @f1
            let f2 = T.pack . symbolVal $ Proxy @f2
            let f3 = T.pack . symbolVal $ Proxy @f3
            -- Not sure we should be inlining the schema. Let
            let nfSchema :: Schema
                nfSchema = mempty
                   { _schemaRequired = [ f1, f2, f3]
                   , _schemaProperties =
                    [(f1, Inline f1Dec)
                    ,(f2, Inline f2Dec)
                    ,(f3, Inline f3Dec)
                    ]
                   }
            pure NamedSchema
                { _namedSchemaName = Just . T.pack . symbolVal $ Proxy @name
                , _namedSchemaSchema = nfSchema
                }


instance ( KnownSymbol name
         , KnownSymbol f1
         , ToSchema a1
         , KnownSymbol f2
         , ToSchema a2
         , KnownSymbol f3
         , ToSchema a3
         , KnownSymbol f4
         , ToSchema a4
         )
    =>  ToSchema (NF4 name f1 a1 f2 a2 f3 a3 f4 a4) where
        declareNamedSchema _ = do
            f1Dec <- declareSchema $ Proxy @a1
            f2Dec <- declareSchema $ Proxy @a2
            f3Dec <- declareSchema $ Proxy @a3
            f4Dec <- declareSchema $ Proxy @a4
            let f1 = T.pack . symbolVal $ Proxy @f1
            let f2 = T.pack . symbolVal $ Proxy @f2
            let f3 = T.pack . symbolVal $ Proxy @f3
            let f4 = T.pack . symbolVal $ Proxy @f4
            -- Not sure we should be inlining the schema. Let
            let nfSchema :: Schema
                nfSchema = mempty
                   { _schemaRequired = [ f1, f2, f3, f4]
                   , _schemaProperties =
                    [(f1, Inline f1Dec)
                    ,(f2, Inline f2Dec)
                    ,(f3, Inline f3Dec)
                    ,(f4, Inline f4Dec)
                    ]
                   }
            pure NamedSchema
                { _namedSchemaName = Just . T.pack . symbolVal $ Proxy @name
                , _namedSchemaSchema = nfSchema
                }

instance ( KnownSymbol name
         , KnownSymbol f1
         , ToSchema a1
         , KnownSymbol f2
         , ToSchema a2
         , KnownSymbol f3
         , ToSchema a3
         , KnownSymbol f4
         , ToSchema a4
         , KnownSymbol f5
         , ToSchema a5
         )
    =>  ToSchema (NF5 name f1 a1 f2 a2 f3 a3 f4 a4 f5 a5) where
        declareNamedSchema _ = do
            f1Dec <- declareSchema $ Proxy @a1
            f2Dec <- declareSchema $ Proxy @a2
            f3Dec <- declareSchema $ Proxy @a3
            f4Dec <- declareSchema $ Proxy @a4
            f5Dec <- declareSchema $ Proxy @a5
            let f1 = T.pack . symbolVal $ Proxy @f1
            let f2 = T.pack . symbolVal $ Proxy @f2
            let f3 = T.pack . symbolVal $ Proxy @f3
            let f4 = T.pack . symbolVal $ Proxy @f4
            let f5 = T.pack . symbolVal $ Proxy @f5
            -- Not sure we should be inlining the schema. Let
            let nfSchema :: Schema
                nfSchema = mempty
                   { _schemaRequired = [ f1, f2, f3, f4, f5]
                   , _schemaProperties =
                    [(f1, Inline f1Dec)
                    ,(f2, Inline f2Dec)
                    ,(f3, Inline f3Dec)
                    ,(f4, Inline f4Dec)
                    ,(f5, Inline f5Dec)
                    ]
                   }
            pure NamedSchema
                { _namedSchemaName = Just . T.pack . symbolVal $ Proxy @name
                , _namedSchemaSchema = nfSchema
                }

instance ( KnownSymbol name
         , KnownSymbol f1
         , ToSchema a1
         , KnownSymbol f2
         , ToSchema a2
         , KnownSymbol f3
         , ToSchema a3
         , KnownSymbol f4
         , ToSchema a4
         , KnownSymbol f5
         , ToSchema a5
         , KnownSymbol f6
         , ToSchema a6
         )
    =>  ToSchema (NF6 name f1 a1 f2 a2 f3 a3 f4 a4 f5 a5 f6 a6) where
        declareNamedSchema _ = do
            f1Dec <- declareSchema $ Proxy @a1
            f2Dec <- declareSchema $ Proxy @a2
            f3Dec <- declareSchema $ Proxy @a3
            f4Dec <- declareSchema $ Proxy @a4
            f5Dec <- declareSchema $ Proxy @a5
            f6Dec <- declareSchema $ Proxy @a6
            let f1 = T.pack . symbolVal $ Proxy @f1
            let f2 = T.pack . symbolVal $ Proxy @f2
            let f3 = T.pack . symbolVal $ Proxy @f3
            let f4 = T.pack . symbolVal $ Proxy @f4
            let f5 = T.pack . symbolVal $ Proxy @f5
            let f6 = T.pack . symbolVal $ Proxy @f6
            -- Not sure we should be inlining the schema. Let
            let nfSchema :: Schema
                nfSchema = mempty
                   { _schemaRequired = [ f1, f2, f3, f4, f5, f6]
                   , _schemaProperties =
                    [(f1, Inline f1Dec)
                    ,(f2, Inline f2Dec)
                    ,(f3, Inline f3Dec)
                    ,(f4, Inline f4Dec)
                    ,(f5, Inline f5Dec)
                    ,(f6, Inline f6Dec)
                    ]
                   }
            pure NamedSchema
                { _namedSchemaName = Just . T.pack . symbolVal $ Proxy @name
                , _namedSchemaSchema = nfSchema
                }



instance ( KnownSymbol name
         , KnownSymbol f1
         , ToSchema a1
         , KnownSymbol f2
         , ToSchema a2
         , KnownSymbol f3
         , ToSchema a3
         , KnownSymbol f4
         , ToSchema a4
         , KnownSymbol f5
         , ToSchema a5
         , KnownSymbol f6
         , ToSchema a6
         , KnownSymbol f7
         , ToSchema a7
         )
    =>  ToSchema (NF7 name f1 a1 f2 a2 f3 a3 f4 a4 f5 a5 f6 a6 f7 a7) where
        declareNamedSchema _ = do
            f1Dec <- declareSchema $ Proxy @a1
            f2Dec <- declareSchema $ Proxy @a2
            f3Dec <- declareSchema $ Proxy @a3
            f4Dec <- declareSchema $ Proxy @a4
            f5Dec <- declareSchema $ Proxy @a5
            f6Dec <- declareSchema $ Proxy @a6
            f7Dec <- declareSchema $ Proxy @a7
            let f1 = T.pack . symbolVal $ Proxy @f1
            let f2 = T.pack . symbolVal $ Proxy @f2
            let f3 = T.pack . symbolVal $ Proxy @f3
            let f4 = T.pack . symbolVal $ Proxy @f4
            let f5 = T.pack . symbolVal $ Proxy @f5
            let f6 = T.pack . symbolVal $ Proxy @f6
            let f7 = T.pack . symbolVal $ Proxy @f7
            -- Not sure we should be inlining the schema. Let
            let nfSchema :: Schema
                nfSchema = mempty
                   { _schemaRequired = [ f1, f2, f3, f4, f5, f6, f7]
                   , _schemaProperties =
                    [(f1, Inline f1Dec)
                    ,(f2, Inline f2Dec)
                    ,(f3, Inline f3Dec)
                    ,(f4, Inline f4Dec)
                    ,(f5, Inline f5Dec)
                    ,(f6, Inline f6Dec)
                    ,(f7, Inline f7Dec)
                    ]
                   }
            pure NamedSchema
                { _namedSchemaName = Just . T.pack . symbolVal $ Proxy @name
                , _namedSchemaSchema = nfSchema
                }


instance ( KnownSymbol name
         , KnownSymbol f1
         , ToSchema a1
         , KnownSymbol f2
         , ToSchema a2
         , KnownSymbol f3
         , ToSchema a3
         , KnownSymbol f4
         , ToSchema a4
         , KnownSymbol f5
         , ToSchema a5
         , KnownSymbol f6
         , ToSchema a6
         , KnownSymbol f7
         , ToSchema a7
         , KnownSymbol f8
         , ToSchema a8
         )
    =>  ToSchema (NF8 name f1 a1 f2 a2 f3 a3 f4 a4 f5 a5 f6 a6 f7 a7 f8 a8) where
        declareNamedSchema _ = do
            f1Dec <- declareSchema $ Proxy @a1
            f2Dec <- declareSchema $ Proxy @a2
            f3Dec <- declareSchema $ Proxy @a3
            f4Dec <- declareSchema $ Proxy @a4
            f5Dec <- declareSchema $ Proxy @a5
            f6Dec <- declareSchema $ Proxy @a6
            f7Dec <- declareSchema $ Proxy @a7
            f8Dec <- declareSchema $ Proxy @a8
            let f1 = T.pack . symbolVal $ Proxy @f1
            let f2 = T.pack . symbolVal $ Proxy @f2
            let f3 = T.pack . symbolVal $ Proxy @f3
            let f4 = T.pack . symbolVal $ Proxy @f4
            let f5 = T.pack . symbolVal $ Proxy @f5
            let f6 = T.pack . symbolVal $ Proxy @f6
            let f7 = T.pack . symbolVal $ Proxy @f7
            let f8 = T.pack . symbolVal $ Proxy @f8
            -- Not sure we should be inlining the schema. Let
            let nfSchema :: Schema
                nfSchema = mempty
                   { _schemaRequired = [ f1, f2, f3, f4, f5, f6, f7, f8]
                   , _schemaProperties =
                    [(f1, Inline f1Dec)
                    ,(f2, Inline f2Dec)
                    ,(f3, Inline f3Dec)
                    ,(f4, Inline f4Dec)
                    ,(f5, Inline f5Dec)
                    ,(f6, Inline f6Dec)
                    ,(f7, Inline f7Dec)
                    ,(f8, Inline f8Dec)
                    ]
                   }
            pure NamedSchema
                { _namedSchemaName = Just . T.pack . symbolVal $ Proxy @name
                , _namedSchemaSchema = nfSchema
                }



instance ( KnownSymbol name
         , KnownSymbol f1
         , ToSchema a1
         , KnownSymbol f2
         , ToSchema a2
         , KnownSymbol f3
         , ToSchema a3
         , KnownSymbol f4
         , ToSchema a4
         , KnownSymbol f5
         , ToSchema a5
         , KnownSymbol f6
         , ToSchema a6
         , KnownSymbol f7
         , ToSchema a7
         , KnownSymbol f8
         , ToSchema a8
         , KnownSymbol f9
         , ToSchema a9
         )
    =>  ToSchema (NF9 name f1 a1 f2 a2 f3 a3 f4 a4 f5 a5 f6 a6 f7 a7 f8 a8 f9 a9) where
        declareNamedSchema _ = do
            f1Dec <- declareSchema $ Proxy @a1
            f2Dec <- declareSchema $ Proxy @a2
            f3Dec <- declareSchema $ Proxy @a3
            f4Dec <- declareSchema $ Proxy @a4
            f5Dec <- declareSchema $ Proxy @a5
            f6Dec <- declareSchema $ Proxy @a6
            f7Dec <- declareSchema $ Proxy @a7
            f8Dec <- declareSchema $ Proxy @a8
            f9Dec <- declareSchema $ Proxy @a9
            let f1 = T.pack . symbolVal $ Proxy @f1
            let f2 = T.pack . symbolVal $ Proxy @f2
            let f3 = T.pack . symbolVal $ Proxy @f3
            let f4 = T.pack . symbolVal $ Proxy @f4
            let f5 = T.pack . symbolVal $ Proxy @f5
            let f6 = T.pack . symbolVal $ Proxy @f6
            let f7 = T.pack . symbolVal $ Proxy @f7
            let f8 = T.pack . symbolVal $ Proxy @f8
            let f9 = T.pack . symbolVal $ Proxy @f9
            -- Not sure we should be inlining the schema. Let
            let nfSchema :: Schema
                nfSchema = mempty
                   { _schemaRequired = [ f1, f2, f3, f4, f5, f6, f7, f8, f9]
                   , _schemaProperties =
                    [(f1, Inline f1Dec)
                    ,(f2, Inline f2Dec)
                    ,(f3, Inline f3Dec)
                    ,(f4, Inline f4Dec)
                    ,(f5, Inline f5Dec)
                    ,(f6, Inline f6Dec)
                    ,(f7, Inline f7Dec)
                    ,(f8, Inline f8Dec)
                    ,(f9, Inline f9Dec)
                    ]
                   }
            pure NamedSchema
                { _namedSchemaName = Just . T.pack . symbolVal $ Proxy @name
                , _namedSchemaSchema = nfSchema
                }














































