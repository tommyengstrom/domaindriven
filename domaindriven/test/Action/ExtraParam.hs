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
    deriving (HasApiOptions)

handleExtraParamAction :: MonadThrow m => ActionHandler () () m (ExtraParamAction ep)
handleExtraParamAction = \case
    ReverseText t -> CbCmd $ \_runTransaction -> pure (T.reverse t)

$(mkServerConfig "extraParamConfig")
