{-# LANGUAGE TemplateHaskell #-}

module Action.ExtraParam where

import Control.Monad.Catch
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Text qualified as T
import DomainDriven
import GHC.Generics (Generic)
import Prelude

data ExtraP = This | That

newtype MyInt (ep :: ExtraP) = MyInt Int
    deriving (Show, Generic)
    deriving newtype (FromJSON, ToJSON)

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
    SayMyNumber :: P x "int" (MyInt ep) -> Sub1Action ep x Cmd String
    deriving (HasApiOptions)

data Sub2Action :: Action where
    SaySomething
        :: P x "number" Int
        -> Sub2Action x Query String
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
    SayMyNumber (MyInt i) -> Cmd $ \_ -> pure (const $ show i, [])

handleSub2Action :: MonadThrow m => ActionHandler () () m Sub2Action
handleSub2Action = \case
    SaySomething i -> Query $ \_ -> pure $ "Something " <> show i

handleSub3Action :: MonadThrow m => ActionHandler () () m (Sub3Action ep bool)
handleSub3Action = \case
    SayHello -> Query $ \_ -> pure "Hello"

$(mkServerConfig "extraParamConfig")
