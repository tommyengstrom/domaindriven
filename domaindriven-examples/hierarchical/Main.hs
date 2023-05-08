module Main where

import Data.Aeson
import Data.Generics.Labels ()
import DomainDriven
    ( ApiTagFromLabel (..)
    , Cmd
    , CmdServer (..)
    , DomainDrivenApi
    , DomainDrivenServer (..)
    , Event
    , Model
    , Persistence (..)
    , Query
    , QueryServer (..)
    , Stored (..)
    , WriteModel
    , mapEvent
    , mapModel
    )
import DomainDriven.Persistance.ForgetfulInMemory (createForgetful)

import GHC.Generics (Generic)
import Lens.Micro
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Server.Generic
import Prelude

--------------------------------------------------------------------------------
-- 1. Define the model
--------------------------------------------------------------------------------

-- | This does nothing interesting, only carries the sub-models.
data FullModel = FullModel
    { numberModel :: NumberModel
    , textModel :: TextModel
    }
    deriving (Show, Eq, Generic)

newtype NumberModel = NumberModel {getNumber :: Int}
    deriving (Show, Eq, Generic)

newtype TextModel = TextModel {getText :: String}
    deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- 2. Define events and how to apply them
--------------------------------------------------------------------------------
data FullEvent
    = NumberEvent NumberEvent
    | TextEvent TextEvent
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

data NumberEvent
    = SetNumber Int
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

data TextEvent
    = SetText String
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

applyEvent :: FullModel -> Stored FullEvent -> FullModel
applyEvent m (Stored ev _ _) = case ev of
    NumberEvent ev' -> m & #numberModel %~ (`applyNumberEvent` ev')
    TextEvent ev' -> m & #textModel %~ (`applyTextEvent` ev')

applyNumberEvent :: NumberModel -> NumberEvent -> NumberModel
applyNumberEvent _ ev = case ev of
    SetNumber i -> NumberModel i

applyTextEvent :: TextModel -> TextEvent -> TextModel
applyTextEvent _ ev = case ev of
    SetText t -> TextModel t

--------------------------------------------------------------------------------
-- 3. Define the API, i.e. the commands and queries
-- If you want to use different model and/or event then these needs to be
-- parameters of all the APIs.
--------------------------------------------------------------------------------
data NumberApi model event mode = NumberApi
    { set :: mode :- ReqBody '[JSON] Int :> Cmd model event Int
    , get :: mode :- Query model Int
    }
    deriving (Generic, ApiTagFromLabel)

data TextApi model event mode = TextApi
    { set :: mode :- ReqBody '[JSON] String :> Cmd model event String
    , get :: mode :- Query model String
    }
    deriving (Generic, ApiTagFromLabel)

data FullApi model event mode = FullApi
    { number :: mode :- DomainDrivenApi NumberApi model event
    , text :: mode :- DomainDrivenApi TextApi model event
    }
    deriving (Generic, ApiTagFromLabel)

-- 3. Implement the endpoints

numberServer :: Monad m => NumberApi NumberModel NumberEvent (AsServerT m)
numberServer =
    NumberApi
        { set = \i -> Cmd $ \_ -> pure (getNumber, [SetNumber i])
        , get = Query (pure . getNumber)
        }

textServer :: Monad m => TextApi TextModel TextEvent (AsServerT m)
textServer =
    TextApi
        { set = \t -> Cmd $ \_ -> pure (getText, [SetText t])
        , get = Query (pure . getText)
        }

fullServer :: Monad m => FullApi FullModel FullEvent (AsServerT m)
fullServer =
    FullApi
        { number =
            mapModel (^. #numberModel)
                . mapEvent NumberEvent
                $ DomainDrivenServer numberServer
        , text =
            mapModel (^. #textModel)
                . mapEvent TextEvent
                $ DomainDrivenServer textServer
        }

fullServer'
    :: Monad m => ServerT (DomainDrivenApi FullApi FullModel FullEvent) m
fullServer' = DomainDrivenServer fullServer

app
    :: ( Model p ~ FullModel
       , Event p ~ FullEvent
       , WriteModel p
       )
    => p
    -> Application
app p =
    serveWithContext
        (Proxy @(DomainDrivenApi FullApi FullModel FullEvent))
        (Persistence p :. EmptyContext)
        fullServer'

main :: IO ()
main = do
    let port = 7878
    putStrLn $ "Running on port " <> show port
    p <- createForgetful applyEvent (FullModel (NumberModel 0) (TextModel ""))
    run port (app p)
