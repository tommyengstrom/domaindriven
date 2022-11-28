{-# LANGUAGE TemplateHaskell #-}

module DomainDriven.ServerSpecModel where

import Control.Monad.Catch
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import DomainDriven
import DomainDriven.Config
import Prelude

data TestAction x method a where
    ReverseText
        :: P x "text" Text
        -> TestAction x CbCmd Text
    ConcatText
        :: P x "text" Text
        -> P x "string" String
        -> TestAction x Query Text
    SubAction
        :: P x "text" Text
        -> SubAction x method a
        -> TestAction x method a
    deriving (HasApiOptions)

handleAction :: MonadThrow m => ActionHandler () () m TestAction
handleAction = \case
    ReverseText t -> CbCmd $ \_runTransaction -> pure (T.reverse t)
    ConcatText a b -> Query $ \() -> pure $ a <> T.pack b
    SubAction t action -> handleSubAction t action

data SubAction x method a where
    Intersperse :: P x "intersperse_text" Text -> SubAction x Query Text
    deriving (HasApiOptions)

handleSubAction :: MonadThrow m => Text -> ActionHandler () () m SubAction
handleSubAction t1 = \case
    Intersperse c -> Query $ \() -> pure $ L.foldl' (flip T.intersperse) t1 (T.unpack c)

$(mkServerConfig "testActionConfig")
