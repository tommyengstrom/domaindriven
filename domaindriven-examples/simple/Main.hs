module Main where

import Data.Aeson
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
    )
import DomainDriven.Persistance.ForgetfulInMemory (createForgetful)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Server.Generic
import Prelude

--------------------------------------------------------------------------------
-- 1. Define the model
--------------------------------------------------------------------------------
newtype CounterModel = CounterModel {getCounter :: Int}
    deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- 2. Define events and how to apply them
--------------------------------------------------------------------------------
data CounterEvent
    = Increase
    | Decrease
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

applyEvent :: CounterModel -> Stored CounterEvent -> CounterModel
applyEvent (CounterModel i) (Stored ev _ _) = CounterModel $ case ev of
    Increase -> i + 1
    Decrease -> i - 1

--------------------------------------------------------------------------------
-- 3. Define the API, i.e. the commands and queries
--------------------------------------------------------------------------------
data CounterApi model event mode = CounterApi
    { current :: mode :- Query model Int
    , increase :: mode :- Cmd model event Int
    , decrease :: mode :- Cmd model event Int
    }
    deriving (Generic)

-- 3. Implement the endpoints
counterServers :: forall m. Monad m => CounterApi CounterModel CounterEvent (AsServerT m)
counterServers =
    CounterApi
        { current = Query (pure . getCounter)
        , increase = Cmd $ \_ -> pure (getCounter, [Increase])
        , decrease = Cmd $ \_ -> pure (getCounter, [Decrease])
        }

-- 4. Define the final API type using `DomainDrivenApi`, which uses the labels of the
-- record to add a path piece to the final endpoints.

type ServantCounterApi = DomainDrivenApi CounterApi CounterModel CounterEvent

-- 5. Define the server.
-- The `HasServer` instance for `DomainDrivenApi` with covert it into a `DomainDrivenServer`
counterServer :: forall m. Monad m => ServerT ServantCounterApi m
counterServer = DomainDrivenServer counterServers

-- FIXME: This should be the default implementation imo
instance ApiTagFromLabel CounterApi where
    apiTagFromLabel = id

app
    :: Model p ~ CounterModel
    => Event p ~ CounterEvent
    => WriteModel p
    => p
    -> Application
app p =
    serveWithContext
        (Proxy @ServantCounterApi)
        (Persistence p :. EmptyContext)
        counterServer

main :: IO ()
main = do
    let port = 7878
    putStrLn $ "Running on port " <> show port
    p <- createForgetful applyEvent (CounterModel 0)
    run port (app p)
