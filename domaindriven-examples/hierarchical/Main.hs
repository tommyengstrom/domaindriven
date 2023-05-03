module Main where

import Data.Aeson
import Data.Generics.Labels ()
import DomainDriven (Event, Model, Stored (..), WriteModel)
import DomainDriven.Persistance.ForgetfulInMemory (createForgetful)
import DomainDriven.Server
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
    { number :: mode :- DomainDrivenApi model event (NumberApi model event)
    , text :: mode :- DomainDrivenApi model event (TextApi model event)
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
            mapModelAndEvent
                (^. #numberModel)
                NumberEvent
                (RecordOfServers numberServer)
        , text =
            mapModelAndEvent
                (^. #textModel)
                TextEvent
                (RecordOfServers textServer)
        }

fullServer'
    :: Monad m => ServerT (DomainDrivenApi FullModel FullEvent (FullApi FullModel FullEvent)) m
fullServer' = RecordOfServers fullServer

app
    :: ( Model p ~ FullModel
       , Event p ~ FullEvent
       , WriteModel p
       )
    => p
    -> Application
app p =
    serveWithContext
        (Proxy @(DomainDrivenApi FullModel FullEvent (FullApi FullModel FullEvent)))
        (Persistence p :. EmptyContext)
        fullServer'

main :: IO ()
main = do
    let port = 7878
    putStrLn $ "Running on port " <> show port
    p <- createForgetful applyEvent (FullModel (NumberModel 0) (TextModel ""))
    run port (app p)
