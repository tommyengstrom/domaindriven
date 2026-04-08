{-# LANGUAGE OverloadedRecordDot #-}

module DomainDriven.Persistance.Postgres.BeamSpec where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Int (Int32)
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Data.Time (diffUTCTime, getCurrentTime)
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
import Database.Beam.Postgres (Pg, Postgres)
import Database.PostgreSQL.Simple
    ( ConnectInfo (..)
    , Connection
    , SqlError
    , close
    , connect
    , execute
    , execute_
    )
import DomainDriven.Persistance.Class
import DomainDriven.Persistance.Postgres
import DomainDriven.Persistance.Postgres.Internal (getEventTableName)
import DomainDriven.Persistance.Postgres.Migration (migrate1to1)
import DomainDriven.Persistance.Postgres.Types (quoteIdent)
import Test.Hspec
import UnliftIO (forConcurrently)
import UnliftIO.Pool (Pool, withResource)
import Prelude

eventTable :: EventTable
eventTable = InitialVersion "beam_backend_events"

eventTableMigrated :: EventTable
eventTableMigrated =
    MigrateUsing migrateEventTable eventTable

legacyEventTable :: EventTable
legacyEventTable = InitialVersion "legacy_backend_events"

data TestEvent
    = AddOne
    | SubtractOne
    | Reset
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

type TestModel = Int

applyTestEvent :: TestModel -> Stored TestEvent -> TestModel
applyTestEvent model storedEvent = case storedEvent.storedEvent of
    AddOne -> model + 1
    SubtractOne -> model - 1
    Reset -> 0

data ProjectionRowT f = ProjectionRow
    { projectionIndex :: Columnar f Text
    , projectionValue :: Columnar f Int32
    }
    deriving stock (Generic)
    deriving anyclass (Beamable)

instance Table ProjectionRowT where
    data PrimaryKey ProjectionRowT f = ProjectionRowId (Columnar f Text)
        deriving stock (Generic)
        deriving anyclass (Beamable)
    primaryKey row = ProjectionRowId row.projectionIndex

type ProjectionRow = ProjectionRowT Identity

data ProjectionDb f = ProjectionDb
    { projectionRows :: f (TableEntity ProjectionRowT)
    }
    deriving stock (Generic)
    deriving anyclass (Database be)

checkedProjectionDb :: CheckedDatabaseSettings Postgres ProjectionDb
checkedProjectionDb =
    defaultMigratableDbSettings
        `withDbModification` dbModification
            { projectionRows =
                renameCheckedEntity (const "projectionRows")
                    <> modifyCheckedTable
                        id
                        checkedTableModification
                            { projectionIndex = checkedFieldNamed "projectionIndex"
                            , projectionValue = checkedFieldNamed "projectionValue"
                            }
            }

projectionDbSettings :: DatabaseSettings Postgres ProjectionDb
projectionDbSettings = unCheckDatabase checkedProjectionDb

beamProjectionSpec :: BeamProjectionSpec ProjectionDb TestModel Indexed TestEvent
beamProjectionSpec =
    BeamProjectionSpec
        { checkedDb = checkedProjectionDb
        , dropProjection = dropProjectionTables
        , applyModelEvent = applyTestEvent
        , loadProjectionModel = loadProjectionModelQuery
        , projectStoredEvent = projectStoredEventQuery
        }

createBeamBackend
    :: Pool Connection
    -> EventTable
    -> IO (PostgresBeam Indexed ProjectionDb TestModel TestEvent)
createBeamBackend pool events =
    postgresBeamBackend pool events beamProjectionSpec

dropProjectionTables :: Connection -> IO ()
dropProjectionTables conn =
    void $
        execute_
            conn
            ("drop table if exists " <> quoteIdent "projectionRows")

loadProjectionModelQuery
    :: DatabaseSettings Postgres ProjectionDb
    -> Indexed
    -> Pg TestModel
loadProjectionModelQuery db index =
    fromMaybe 0 . fmap rowToModel <$> loadProjectionRow db index

projectStoredEventQuery
    :: DatabaseSettings Postgres ProjectionDb
    -> Indexed
    -> Stored TestEvent
    -> Pg ()
projectStoredEventQuery db index storedEvent = do
    currentRow <- loadProjectionRow db index
    let nextModel =
            applyTestEvent
                (fromMaybe 0 $ fmap rowToModel currentRow)
                storedEvent
        nextRow = rowFromModel index nextModel
    case currentRow of
        Nothing ->
            runInsert $
                insert
                    (projectionRows db)
                    (insertValues [nextRow])
        Just _ ->
            runUpdate $
                save
                    (projectionRows db)
                    nextRow

loadProjectionRow
    :: DatabaseSettings Postgres ProjectionDb
    -> Indexed
    -> Pg (Maybe ProjectionRow)
loadProjectionRow db index =
    runSelectReturningOne $
        select $ do
            row <- all_ (projectionRows db)
            guard_ (row.projectionIndex ==. val_ (toPgIndex index))
            pure row

rowToModel :: ProjectionRow -> TestModel
rowToModel row = fromIntegral row.projectionValue

rowFromModel :: Indexed -> TestModel -> ProjectionRow
rowFromModel index model =
    ProjectionRow
        { projectionIndex = toPgIndex index
        , projectionValue = fromIntegral model
        }

migrateEventTable
    :: PreviousEventTableName
    -> EventTableName
    -> Connection
    -> IO ()
migrateEventTable previousTable nextTable conn =
    migrate1to1 @Indexed @Value conn previousTable nextTable id

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
dropArtifacts conn =
    mapM_ dropTableIfExists tablesToDrop
  where
    tablesToDrop =
        [ "projectionRows"
        , getEventTableName eventTable
        , getEventTableName eventTable <> "_projection_meta"
        , getEventTableName eventTableMigrated
        , getEventTableName eventTableMigrated <> "_projection_meta"
        , getEventTableName legacyEventTable
        ]

    dropTableIfExists :: String -> IO ()
    dropTableIfExists tableName =
        void $
            execute_
                conn
                ("drop table if exists " <> quoteIdent tableName <> " cascade")

setupBeam
    :: ((PostgresBeam Indexed ProjectionDb TestModel TestEvent, Pool Connection) -> IO ())
    -> IO ()
setupBeam test = do
    conn <- mkTestConn
    dropArtifacts conn
    close conn
    pool <- simplePool mkTestConn
    backend <- createBeamBackend pool eventTable
    test (withBeamChunkSize 2 backend, pool)

withBeamChunkSize
    :: ChunkSize
    -> PostgresBeam index db model event
    -> PostgresBeam index db model event
withBeamChunkSize newChunkSize PostgresBeam{connectionPool, eventTableName, projectionSpec, updateHook, logger} =
    PostgresBeam
        { connectionPool
        , eventTableName
        , projectionSpec
        , chunkSize = newChunkSize
        , updateHook
        , logger
        }

spec :: Spec
spec = around setupBeam $ do
    describe "PostgresBeam backend" $ do
        it "matches command results, model state and event payloads of PostgresEvent" $ \(beam, pool) -> do
            legacy <- postgresWriteModel pool legacyEventTable applyTestEvent 0
            let index = Indexed "parity"
                script = [AddOne, AddOne, SubtractOne, AddOne]

            beamResults <-
                mapM (\event -> runCmd beam index (\_ -> pure (id, [event]))) script
            legacyResults <-
                mapM (\event -> runCmd legacy index (\_ -> pure (id, [event]))) script

            beamModel <- getModel beam index
            legacyModel <- getModel legacy index
            beamEvents <- fmap storedEvent <$> getEventList beam index
            legacyEvents <- fmap storedEvent <$> getEventList legacy index

            beamResults `shouldBe` legacyResults
            beamModel `shouldBe` legacyModel
            beamEvents `shouldBe` legacyEvents

        it "rolls back the event insert when projection constraints fail" $ \(beam, pool) -> do
            let index = Indexed "guarded"

            withResource pool $ \conn ->
                void $
                    execute_
                        conn
                        "alter table \"projectionRows\" add constraint projection_nonnegative check (\"projectionValue\" >= 0)"

            runCmd beam index (\_ -> pure (id, [SubtractOne]))
                `shouldThrow` (\(_ :: SqlError) -> True)

            getModel beam index `shouldReturn` 0
            events <- getEventList beam index
            fmap storedEvent events `shouldBe` []

        it "rebuildProjection restores the relational projection from the event table" $ \(beam, pool) -> do
            let index = Indexed "rebuild"

            void $ runCmd beam index $ \_ -> pure (id, [AddOne, AddOne, SubtractOne])
            withResource pool $ \conn ->
                void $
                    execute
                        conn
                        "update \"projectionRows\" set \"projectionValue\" = ? where \"projectionIndex\" = ?"
                        (99 :: Int32, toPgIndex index)

            getModel beam index `shouldReturn` 99
            rebuildProjection beam
            getModel beam index `shouldReturn` 1

        it "rebuilds the projection after migrating to a newer event table" $ \(beam, pool) -> do
            let index = Indexed "migration"

            void $ runCmd beam index $ \_ -> pure (id, [AddOne, AddOne, SubtractOne])

            migrated <- createBeamBackend pool eventTableMigrated
            getModel migrated index `shouldReturn` 1
            migratedEvents <- fmap storedEvent <$> getEventList migrated index
            migratedEvents `shouldBe` [AddOne, AddOne, SubtractOne]

        it "allows different indices to execute in parallel" $ \(beam, _pool) -> do
            let command :: TestModel -> IO (TestModel -> TestModel, [TestEvent])
                command _ = do
                    threadDelay 100000
                    pure (id, [AddOne])

            t0 <- getCurrentTime
            models <-
                forConcurrently ([1 .. 10] :: [Int]) $ \i ->
                    runCmd beam (Indexed $ pack ("parallel-" <> show i)) command
            t1 <- getCurrentTime

            models `shouldBe` replicate 10 1
            diffUTCTime t1 t0 `shouldSatisfy` (< 1.5)

        it "serializes commands for the same index" $ \(beam, _pool) -> do
            let index = Indexed "sequential"
                command :: TestModel -> IO (TestModel -> TestModel, [TestEvent])
                command _ = do
                    threadDelay 100000
                    pure (id, [AddOne])

            t0 <- getCurrentTime
            models <- forConcurrently ([1 .. 10] :: [Int]) $ \_ -> runCmd beam index command
            t1 <- getCurrentTime

            L.sort models `shouldBe` [1 .. 10]
            diffUTCTime t1 t0 `shouldSatisfy` (> 0.9)
