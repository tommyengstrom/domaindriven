{-# LANGUAGE OverloadedRecordDot #-}

module DomainDriven.BeamSpec (spec) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Database.Beam
import Database.Beam.Migrate
    ( CheckedDatabaseSettings
    , checkedFieldNamed
    , checkedTableModification
    , defaultMigratableDbSettings
    , modifyCheckedTable
    , renameCheckedEntity
    , unCheckDatabase
    )
import Database.Beam.Postgres (Connection, Pg, Postgres)
import Database.PostgreSQL.Simple (ConnectInfo (..), close, connect, execute_)
import DomainDriven
import DomainDriven.Persistance.ForgetfulInMemory
import DomainDriven.Persistance.Postgres
import DomainDriven.Persistance.Postgres.Internal (getEventTableName)
import DomainDriven.Persistance.Postgres.Types (quoteIdent)
import Effectful (Eff, IOE, runEff)
import Test.Hspec
import UnliftIO.Pool (Pool)
import Prelude

type TestModel = Int

data TestEvent
    = AddOne
    | SubtractOne
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

type TestDomain = Domain TestModel TestEvent NoIndex

applyTestEvent :: TestModel -> Stored TestEvent -> TestModel
applyTestEvent model storedEvent = case storedEvent.storedEvent of
    AddOne -> model + 1
    SubtractOne -> model - 1

data CounterRowT f = CounterRow
    { counterId :: Columnar f Text
    , counterValue :: Columnar f Int32
    }
    deriving stock (Generic)
    deriving anyclass (Beamable)

instance Table CounterRowT where
    data PrimaryKey CounterRowT f = CounterRowId (Columnar f Text)
        deriving stock (Generic)
        deriving anyclass (Beamable)
    primaryKey row = CounterRowId row.counterId

type CounterRow = CounterRowT Identity

data CounterDb f = CounterDb
    { counterProjection :: f (TableEntity CounterRowT)
    }
    deriving stock (Generic)
    deriving anyclass (Database be)

checkedCounterDb :: CheckedDatabaseSettings Postgres CounterDb
checkedCounterDb =
    defaultMigratableDbSettings
        `withDbModification` dbModification
            { counterProjection =
                renameCheckedEntity (const "beamInterpreterProjection")
                    <> modifyCheckedTable
                        id
                        checkedTableModification
                            { counterId = checkedFieldNamed "counter_id"
                            , counterValue = checkedFieldNamed "counter_value"
                            }
            }

counterDbSettings :: DatabaseSettings Postgres CounterDb
counterDbSettings = unCheckDatabase checkedCounterDb

eventTable :: EventTable
eventTable = InitialVersion "beam_interpreter_events"

beamProjectionSpec :: BeamProjectionSpec CounterDb TestModel NoIndex TestEvent
beamProjectionSpec =
    BeamProjectionSpec
        { checkedDb = checkedCounterDb
        , dropProjection = dropProjectionTables
        , applyModelEvent = applyTestEvent
        , loadProjectionModel = \db _ -> loadCounterQuery db
        , projectStoredEvent = \db _ -> projectCounterEvent db
        }

createBeamBackend :: Pool Connection -> IO (PostgresBeam NoIndex CounterDb TestModel TestEvent)
createBeamBackend pool =
    postgresBeamBackend pool eventTable beamProjectionSpec

dropProjectionTables :: Connection -> IO ()
dropProjectionTables conn =
    void $
        execute_
            conn
            ("drop table if exists " <> quoteIdent "beamInterpreterProjection")

loadCounterQuery :: DatabaseSettings Postgres CounterDb -> Pg TestModel
loadCounterQuery db =
    fromMaybe 0 . fmap rowToModel <$> loadCounterRow db

projectCounterEvent
    :: DatabaseSettings Postgres CounterDb
    -> Stored TestEvent
    -> Pg ()
projectCounterEvent db storedEvent = do
    currentRow <- loadCounterRow db
    let nextModel =
            applyTestEvent
                (fromMaybe 0 $ fmap rowToModel currentRow)
                storedEvent
        nextRow = rowFromModel nextModel
    case currentRow of
        Nothing ->
            runInsert $
                insert
                    (counterProjection db)
                    (insertValues [nextRow])
        Just _ ->
            runUpdate $
                save
                    (counterProjection db)
                    nextRow

loadCounterRow :: DatabaseSettings Postgres CounterDb -> Pg (Maybe CounterRow)
loadCounterRow db =
    runSelectReturningOne $
        select $ do
            row <- all_ (counterProjection db)
            guard_ (row.counterId ==. val_ counterRowKey)
            pure row

counterRowKey :: Text
counterRowKey = "counter"

rowToModel :: CounterRow -> TestModel
rowToModel row = fromIntegral row.counterValue

rowFromModel :: TestModel -> CounterRow
rowFromModel model =
    CounterRow
        { counterId = counterRowKey
        , counterValue = fromIntegral model
        }

mkTestConn :: IO Connection
mkTestConn =
    connect $
        ConnectInfo
            { connectHost = "localhost"
            , connectPort = 5432
            , connectUser = "postgres"
            , connectPassword = "postgres"
            , connectDatabase = "domaindriven"
            }

dropArtifacts :: Connection -> IO ()
dropArtifacts conn = do
    void $
        execute_
            conn
            ("drop table if exists " <> quoteIdent "beamInterpreterProjection")
    void $
        execute_
            conn
            ("drop table if exists " <> quoteIdent (getEventTableName eventTable <> "_projection_meta"))
    void $
        execute_
            conn
            ("drop table if exists " <> quoteIdent (getEventTableName eventTable))

setupBeamBackend
    :: (PostgresBeam NoIndex CounterDb TestModel TestEvent -> IO ())
    -> IO ()
setupBeamBackend test = do
    conn <- mkTestConn
    dropArtifacts conn
    close conn
    pool <- simplePool mkTestConn
    backend <- createBeamBackend pool
    test backend

runBeamCommandTest
    :: PostgresBeam NoIndex CounterDb TestModel TestEvent
    -> Eff
        '[ BeamAggregate TestDomain CounterDb
         , Projection TestDomain
         , IOE
         ]
        a
    -> IO a
runBeamCommandTest backend =
    runEff
        . runProjection backend
        . runBeamAggregate backend

runBeamReadTest
    :: PostgresBeam NoIndex CounterDb TestModel TestEvent
    -> Eff
        '[ BeamProjection CounterDb
         , Projection TestDomain
         , IOE
         ]
        a
    -> IO a
runBeamReadTest backend =
    runEff
        . runProjection backend
        . runBeamProjection backend

runInMemoryTest
    :: ForgetfulInMemory TestModel NoIndex TestEvent
    -> Eff '[Aggregate TestDomain, Projection TestDomain, IOE] a
    -> IO a
runInMemoryTest backend =
    runEff
        . runProjection backend
        . runAggregate backend

withBeamUpdateHook
    :: (PostgresBeam NoIndex CounterDb TestModel TestEvent -> NoIndex -> TestModel -> [Stored TestEvent] -> IO ())
    -> PostgresBeam NoIndex CounterDb TestModel TestEvent
    -> PostgresBeam NoIndex CounterDb TestModel TestEvent
withBeamUpdateHook newUpdateHook PostgresBeam{connectionPool, eventTableName, projectionSpec, chunkSize, logger} =
    PostgresBeam
        { connectionPool
        , eventTableName
        , projectionSpec
        , chunkSize
        , updateHook = newUpdateHook
        , logger
        }

runBeamScript
    :: PostgresBeam NoIndex CounterDb TestModel TestEvent
    -> IO ([TestModel], TestModel, [TestEvent])
runBeamScript backend = do
    results <- runBeamCommandTest backend $ do
        r1 <- runBeamTransaction @TestDomain @CounterDb $ pure (loadCounterQuery counterDbSettings, [AddOne])
        r2 <- runBeamTransaction @TestDomain @CounterDb $ pure (loadCounterQuery counterDbSettings, [AddOne])
        r3 <- runBeamTransaction @TestDomain @CounterDb $ pure (loadCounterQuery counterDbSettings, [SubtractOne])
        pure [r1, r2, r3]
    finalModel <- runBeamCommandTest backend (getModel @TestDomain)
    events <- runBeamCommandTest backend (fmap storedEvent <$> getEventList @TestDomain)
    pure (results, finalModel, events)

runInMemoryScript
    :: ForgetfulInMemory TestModel NoIndex TestEvent
    -> IO ([TestModel], TestModel, [TestEvent])
runInMemoryScript backend = do
    results <- runInMemoryTest backend $ do
        r1 <- runTransaction @TestDomain $ \_ -> pure (id, [AddOne])
        r2 <- runTransaction @TestDomain $ \_ -> pure (id, [AddOne])
        r3 <- runTransaction @TestDomain $ \_ -> pure (id, [SubtractOne])
        pure [r1, r2, r3]
    finalModel <- runInMemoryTest backend (getModel @TestDomain)
    events <- runInMemoryTest backend (fmap storedEvent <$> getEventList @TestDomain)
    pure (results, finalModel, events)

spec :: Spec
spec = around setupBeamBackend $ do
    describe "Beam interpreters" $ do
        it "keeps BeamProjection reads aligned with Projection reads" $ \backend -> do
            result <-
                runBeamCommandTest backend $
                    runBeamTransaction @TestDomain @CounterDb $
                        pure (loadCounterQuery counterDbSettings, [AddOne, AddOne])
            result `shouldBe` 2

            relationalRead <- runBeamReadTest backend $ runPg (loadCounterQuery counterDbSettings)
            projectionRead <- runBeamCommandTest backend (getModel @TestDomain)

            relationalRead `shouldBe` 2
            projectionRead `shouldBe` 2

        it "can inspect relational state inside the Beam command callback" $ \backend -> do
            void $
                runBeamCommandTest backend $
                    runBeamTransaction @TestDomain @CounterDb $
                        pure (loadCounterQuery counterDbSettings, [AddOne])

            result <-
                runBeamCommandTest backend $
                    runBeamTransaction @TestDomain @CounterDb $ do
                        current <- runPg $ loadCounterQuery counterDbSettings
                        let events = if current > 0 then [SubtractOne] else []
                        pure (loadCounterQuery counterDbSettings, events)

            result `shouldBe` 0

        it "preserves the observable command trace of ForgetfulInMemory" $ \backend -> do
            inMemory <- createForgetful applyTestEvent (0 :: TestModel)
            beamTrace <- runBeamScript backend
            inMemoryTrace <- runInMemoryScript inMemory
            beamTrace `shouldBe` inMemoryTrace

        it "fires post-update hooks with the updated model and stored events" $ \backend -> do
            observed <- newEmptyMVar
            let backend' =
                    withBeamUpdateHook
                        (\_ _ model events -> putMVar observed (model, map storedEvent events))
                        backend

            result <-
                runBeamCommandTest backend' $
                    runBeamTransaction @TestDomain @CounterDb $
                        pure (loadCounterQuery counterDbSettings, [AddOne, AddOne])
            result `shouldBe` 2
            takeMVar observed `shouldReturn` (2, [AddOne, AddOne])
