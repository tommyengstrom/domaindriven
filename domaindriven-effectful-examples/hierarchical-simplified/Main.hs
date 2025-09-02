{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Main where

import Data.Aeson
import DomainDriven.Persistance.Class (Stored(..), NoIndex(..))
import DomainDriven.Persistance.ForgetfulInMemory (createForgetful, ForgetfulInMemory)
import DomainDriven.Effectful
import DomainDriven.Effectful.Interpreter.InMemory
import Effectful
import Lens.Micro
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.API.Generic
import Servant.Server.Generic (genericServeT, AsServerT)
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

newtype NumberModel = NumberModel {numberValue :: Int}
    deriving (Show, Eq, Generic)

newtype TextModel = TextModel {textValue :: String}
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
-- 3. Define Domain types - This is the KEY simplification!
--------------------------------------------------------------------------------

-- Define domain configurations - just one type parameter each!
type FullDomain = SimpleDomain FullModel FullEvent
type NumberDomain = SimpleDomain NumberModel NumberEvent
type TextDomain = SimpleDomain TextModel TextEvent

-- Simplified effect constraints using domain types
type FullEffects es = 
    ( ProjectionD FullDomain Effectful.:> es
    , AggregateD FullDomain Effectful.:> es )

type NumberEffects es = 
    ( ProjectionD NumberDomain Effectful.:> es
    , AggregateD NumberDomain Effectful.:> es )

type TextEffects es = 
    ( ProjectionD TextDomain Effectful.:> es
    , AggregateD TextDomain Effectful.:> es )

--------------------------------------------------------------------------------
-- 4. Define the API using record-based approach with standard Servant combinators
--------------------------------------------------------------------------------

data NumberAPI mode = NumberAPI
    { getNumber :: mode Servant.:- Get '[JSON] Int
    , setNumber :: mode Servant.:- ReqBody '[JSON] Int Servant.:> Post '[JSON] Int
    } deriving (Generic)

data TextAPI mode = TextAPI
    { getText :: mode Servant.:- Get '[JSON] String
    , setText :: mode Servant.:- ReqBody '[JSON] String Servant.:> Post '[JSON] String
    } deriving (Generic)

data FullAPI mode = FullAPI
    { number :: mode Servant.:- "number" Servant.:> NamedRoutes NumberAPI
    , text   :: mode Servant.:- "text" Servant.:> NamedRoutes TextAPI
    } deriving (Generic)

--------------------------------------------------------------------------------
-- 5. Implement handlers using Effectful effects with simplified domain API
--------------------------------------------------------------------------------

-- Much cleaner! Just one type parameter instead of three!
numberHandlers :: FullEffects es => NumberAPI (AsServerT (Eff es))
numberHandlers = NumberAPI
    { getNumber = do
        model <- getModelD @FullDomain  -- Single type parameter!
        pure $ numberValue (numberModel model)
    , setNumber = \n -> 
        runTransactionD @FullDomain NoIndex $ \_ ->
            pure (\m -> numberValue (numberModel m), [NumberEvent (SetNumber n)])
    }

textHandlers :: FullEffects es => TextAPI (AsServerT (Eff es))
textHandlers = TextAPI
    { getText = do
        model <- getModelD @FullDomain  -- Single type parameter!
        pure $ textValue (textModel model)
    , setText = \t -> 
        runTransactionD @FullDomain NoIndex $ \_ ->
            pure (\m -> textValue (textModel m), [TextEvent (SetText t)])
    }

fullHandlers :: FullEffects es => FullAPI (AsServerT (Eff es))
fullHandlers = FullAPI
    { number = numberHandlers
    , text = textHandlers
    }

--------------------------------------------------------------------------------
-- 6. Wire up the server with effect interpreters
--------------------------------------------------------------------------------

mkFullServer :: ForgetfulInMemory FullModel NoIndex FullEvent -> Application
mkFullServer backend = 
    genericServeT runEffects fullHandlers
  where
    -- Helper to run effects - using domain-based interpreters
    runEffects :: Eff '[ProjectionD FullDomain,
                       AggregateD FullDomain,
                       IOE] a -> Handler a
    runEffects = liftIO 
               . runEff 
               . runAggregateInMemoryD @FullDomain backend
               . runProjectionInMemoryD @FullDomain backend NoIndex

main :: IO ()
main = do
    let port = 7878
    putStrLn $ "Running Effectful hierarchical example (simplified) on port " <> show port
    
    -- Initialize with default values
    let initialModel = FullModel (NumberModel 0) (TextModel "")
    backend <- createForgetful @NoIndex applyEvent initialModel
    
    -- Create and run the application
    let app = mkFullServer backend
    
    run port app