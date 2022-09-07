{-# LANGUAGE TemplateHaskell #-}
module DomainDriven.ServerSpecModel where

import           Control.Monad.Catch
import qualified Data.Text                                    as T
import           Data.Text                      ( Text )
import           DomainDriven
import           DomainDriven.Config
import           Prelude
import           Servant

data TestAction method a where
    ReverseText ::Text -> TestAction (RequestType '[PlainText] (Verb 'POST 200 '[JSON])) Text
    ConcatText ::Text -> String -> TestAction Query Text
    SubAction ::Text -> SubAction method a -> TestAction method a
    deriving HasApiOptions

data SubAction method a where
    -- Intersperse ::Text -> SubAction Query Text -- Duplicate endpoint
    Intersperse ::Char -> SubAction Query Text
    deriving HasApiOptions

handleTestAction :: MonadThrow m => ActionHandler () () m TestAction
handleTestAction = \case
    ReverseText t       -> Cmd $ \() -> pure (const $ T.reverse t, [])
    ConcatText a b      -> Query $ \() -> pure $ a <> T.pack b
    SubAction  t action -> handleSubAction t action


handleSubAction :: MonadThrow m => Text -> ActionHandler () () m SubAction
handleSubAction t1 = \case
    Intersperse c -> Query $ \() -> pure $ T.intersperse c t1


$(mkServerConfig "testActionConfig")
