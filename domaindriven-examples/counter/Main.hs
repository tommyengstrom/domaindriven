module Main where

import Data.Aeson
import DomainDriven (Event, Model, Stored (..), WriteModel)
import DomainDriven.Persistance.ForgetfulInMemory (createForgetful)
import DomainDriven.Server.Servant
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
data CounterApis mode = CounterApis
    { current :: mode :- Query CounterModel Int
    , increase :: mode :- Cmd CounterModel CounterEvent Int
    , decrease :: mode :- Cmd CounterModel CounterEvent Int
    }
    deriving (Generic)

-- FIXME: Make sure it's good enough to only derive GHC generics!

-- 3. Implement the endpoints
counterServers :: forall m. Monad m => CounterApis (AsServerT m)
counterServers =
    CounterApis
        { current = Query (pure . getCounter)
        , increase = Cmd $ \_ -> pure (getCounter, [Increase])
        , decrease = Cmd $ \_ -> pure (getCounter, [Decrease])
        }

-- 4. Define the final API type using `TaggedSumOfApis`, which uses the labels of the
-- record to add a path piece to the final endpoints.

type CounterApi = TaggedSumOfApis CounterApis

-- 5. Define the server.
-- The `HasServer` instance for `TaggedSumOfApis` with covert it into a `RecordOfServers`
counterServer :: forall m. Monad m => ServerT CounterApi m
counterServer = RecordOfServers counterServers

-- FIXME: This should be the default implementation imo
instance ApiTagFromLabel CounterApis where
    apiTagFromLabel = id

app
    :: Model p ~ CounterModel
    => Event p ~ CounterEvent
    => WriteModel p
    => p
    -> Application
app p =
    serveWithContext
        (Proxy @CounterApi)
        (Persistence p :. EmptyContext)
        counterServer

main :: IO ()
main = do
    let port = 7878
    putStrLn $ "Running on port " <> show port
    p <- createForgetful applyEvent (CounterModel 0)
    run port (app p)
