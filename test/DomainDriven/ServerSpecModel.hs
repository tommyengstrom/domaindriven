{-# LANGUAGE TemplateHaskell #-}
module DomainDriven.ServerSpecModel where

import qualified Data.Text                                    as T
import           Data.Text                      ( Text )
import           DomainDriven
import           DomainDriven.Config
import           DomainDriven.Server
import           Prelude
import           Servant

data TestAction method a where
    ReverseText ::Text -> TestAction (RequestType '[PlainText] (Verb 'POST 200 '[JSON])) Text
    deriving HasApiOptions

handleTestAction :: ActionHandler () () TestAction
handleTestAction = \case
    ReverseText t -> Cmd $ \() -> pure (T.reverse t, [])


$(mkServerConfig "testActionConfig")