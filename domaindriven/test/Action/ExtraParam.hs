{-# LANGUAGE TemplateHaskell #-}

module Action.ExtraParam where

import Control.Monad.Catch
import Data.Text (Text)
import Data.Text qualified as T
import DomainDriven
import Prelude

data ExtraP = This | That

data ExtraParamAction (ep :: ExtraP) :: Action where
    ReverseText
        :: P x "text" Text
        -> ExtraParamAction ep x CbCmd Text
    Sub1 :: Sub1Action ep x a r -> ExtraParamAction ep x a r
    -- Sub2 :: Sub2Action x a r -> ExtraParamAction ep x a r
    deriving (HasApiOptions)

data Sub1Action (ep :: ExtraP) :: Action where
    TalkToMe :: Sub1Action ep x Query Text
    TellMe :: Sub1Action ep x Cmd Text
    deriving (HasApiOptions)

data Sub2Action :: Action where
    SaySomething :: Sub2Action x Query Text
    deriving (HasApiOptions)

handleExtraParamAction :: MonadThrow m => ActionHandler () () m (ExtraParamAction ep)
handleExtraParamAction = \case
    ReverseText t -> CbCmd $ \_runTransaction -> pure (T.reverse t)
    Sub1 a -> handleSub1Action a

-- Sub2 a -> handleSub2Action a

handleSub1Action :: MonadThrow m => ActionHandler () () m (Sub1Action ep)
handleSub1Action = \case
    TalkToMe -> Query $ \_ -> pure "Hello!"
    TellMe -> Cmd $ \_ -> pure (const "hey", [])

handleSub2Action :: MonadThrow m => ActionHandler () () m Sub2Action
handleSub2Action = \case
    SaySomething -> Query $ \_ -> pure "Something"

$(mkServerConfig "extraParamConfig")
