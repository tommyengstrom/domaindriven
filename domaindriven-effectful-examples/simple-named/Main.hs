{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad (when)
import Data.Aeson
import DomainDriven.Effectful
import DomainDriven.Effectful.Interpreter.InMemory
import DomainDriven.FieldNameAsPath
import DomainDriven.Persistance.Class (NoIndex (..), Stored (..))
import DomainDriven.Persistance.ForgetfulInMemory (ForgetfulInMemory, createForgetful)
import Effectful hiding ((:>))
import Effectful qualified
import Effectful.Error.Static
import Network.Wai.Handler.Warp (run)
import Servant hiding (throwError)
import Servant qualified
import Servant.API.Generic
import Servant.Server.Generic (AsServerT)
import Prelude

--------------------------------------------------------------------------------
-- 1. Define the model
--------------------------------------------------------------------------------
type CounterModel = Int

--------------------------------------------------------------------------------
-- 2. Define events and how to apply them
--------------------------------------------------------------------------------
data CounterEvent
    = Increase
    | Decrease
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

applyEvent :: CounterModel -> Stored CounterEvent -> CounterModel
applyEvent i (Stored ev _ _) = case ev of
    Increase -> i + 1
    Decrease -> i - 1

type CounterDomain = Domain CounterModel CounterEvent NoIndex

--------------------------------------------------------------------------------
-- 3. Define the Servant API
--------------------------------------------------------------------------------
data CounterAPI mode = CounterAPI
    { get :: mode :- Get '[JSON] Int
    , increase :: mode :- "increase" :> Post '[JSON] Int
    , decrease :: mode :- "decrease" :> Post '[JSON] Int
    }
    deriving (Generic, ApiTagFromLabel)

--------------------------------------------------------------------------------
-- 4. Implement the server handlers using Effectful effects
--------------------------------------------------------------------------------

-- | Counter handlers using Effectful effects
counterServer
    :: ( Projection CounterDomain Effectful.:> es
       , Aggregate CounterDomain Effectful.:> es
       , Error ServerError Effectful.:> es
       )
    => CounterAPI (AsServerT (Eff es))

counterServer =
    CounterAPI
        { get = getModel
        , increase = runTransaction do
            pure (id, [Increase])
        , decrease = runTransaction do
            m <- getModel
            when (id m <= 0)
                . throwError
                $ err422{errBody = "Counter cannot go below zero"}
            pure (id, [Decrease])
        }

counterServer'
    :: ( Projection CounterDomain Effectful.:> es
       , Aggregate CounterDomain Effectful.:> es
       , Error ServerError Effectful.:> es
       )
    => ServerT (FieldNameAsPathApi CounterAPI) (Eff es)
counterServer' = FieldNameAsPathServer counterServer

--------------------------------------------------------------------------------
-- 5. Wire up the server with effect interpreters
--------------------------------------------------------------------------------

mkCounterServer' :: ForgetfulInMemory CounterModel NoIndex CounterEvent -> Application
mkCounterServer' backend =
    serveWithContextT
        (Proxy @(FieldNameAsPathApi CounterAPI))
        EmptyContext
        runEffects
        counterServer'
  where
    -- Helper to run effects and convert to Handler
    runEffects
        :: Eff
            '[ Projection CounterDomain
             , Aggregate CounterDomain
             , Error ServerError
             , IOE
             ]
            a
        -> Handler a
    runEffects m = do
        a <-
            liftIO
                . runEff
                . runErrorNoCallStack @ServerError
                . runAggregateInMemory backend
                $ runProjectionInMemory backend m
        either Servant.throwError pure a


main :: IO ()
main = do
    let port = 7878
    putStrLn $ "Running Effectful counter on port " <> show port

    -- Initialize the in-memory backend
    backend <- createForgetful @NoIndex applyEvent 0

    -- Create and run the application
    let app = mkCounterServer' backend

    run port app

