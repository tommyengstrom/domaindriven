module DomainDriven.Internal.Text where

import Data.Char
    ( toLower
    , toUpper
    )
import Data.Text (Text)
import qualified Data.Text as T
import Prelude

lowerFirst :: String -> String
lowerFirst = \case
    [] -> []
    c : cs -> toLower c : cs

lowerFirstT :: Text -> Text
lowerFirstT = T.pack . lowerFirst . T.unpack

upperFirst :: String -> String
upperFirst = \case
    [] -> []
    c : cs -> toUpper c : cs

upperFirstT :: Text -> Text
upperFirstT = T.pack . upperFirst . T.unpack

camelAppend :: String -> String -> String
camelAppend a b = a <> upperFirst b

camelAppendT :: Text -> Text -> Text
camelAppendT a b = a <> upperFirstT b
