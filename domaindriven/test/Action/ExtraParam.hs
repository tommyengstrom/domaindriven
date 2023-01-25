{-# LANGUAGE TemplateHaskell #-}

module Action.ExtraParam where

import Control.Monad.Catch
import Data.Text (Text)
import Data.Text qualified as T
import DomainDriven
import Prelude

data ExtraP = This | That

data ExtraParamAction (bool :: Bool) (ep :: ExtraP) :: Action where
    ReverseText
        :: P x "text" Text
        -> ExtraParamAction bool ep x CbCmd Text
    Sub1 :: Sub1Action ep x a r -> ExtraParamAction bool ep x a r
    Sub2 :: Sub2Action x a r -> ExtraParamAction bool ep x a r
    Sub3 :: Sub3Action ep bool x a r -> ExtraParamAction bool ep x a r
    deriving (HasApiOptions)

data Sub1Action (ep :: ExtraP) :: Action where
    TalkToMe :: Sub1Action ep x Query Text
    TellMe :: Sub1Action ep x Cmd Text
    deriving (HasApiOptions)

data Sub2Action :: Action where
    SaySomething :: Sub2Action x Query Text
    deriving (HasApiOptions)

data Sub3Action (ep :: ExtraP) (bool :: Bool) :: Action where
    SayHello :: Sub3Action ep bool x Query Text
    deriving (HasApiOptions)

handleExtraParamAction :: MonadThrow m => ActionHandler () () m (ExtraParamAction bool ep)
handleExtraParamAction = \case
    ReverseText t -> CbCmd $ \_runTransaction -> pure (T.reverse t)
    Sub1 a -> handleSub1Action a
    Sub2 a -> handleSub2Action a
    Sub3 a -> handleSub3Action a

handleSub1Action :: MonadThrow m => ActionHandler () () m (Sub1Action ep)
handleSub1Action = \case
    TalkToMe -> Query $ \_ -> pure "Hello!"
    TellMe -> Cmd $ \_ -> pure (const "hey", [])

handleSub2Action :: MonadThrow m => ActionHandler () () m Sub2Action
handleSub2Action = \case
    SaySomething -> Query $ \_ -> pure "Something"

handleSub3Action :: MonadThrow m => ActionHandler () () m (Sub3Action ep bool)
handleSub3Action = \case
    SayHello -> Query $ \_ -> pure "Hello"

$(mkServerConfig "extraParamConfig")
