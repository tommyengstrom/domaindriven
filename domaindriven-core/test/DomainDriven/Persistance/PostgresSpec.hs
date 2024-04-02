module DomainDriven.Persistance.PostgresSpec where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException)
import Control.Monad
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Foldable
import Data.String (fromString)
import Data.Time
import Data.Traversable
import Data.UUID (nil)
import Data.UUID.V4 qualified as V4
import Database.PostgreSQL.Simple
import DomainDriven.Persistance.Class
import DomainDriven.Persistance.Postgres
import DomainDriven.Persistance.Postgres.Internal
    ( getEventTableName
    , queryEvents
    , writeEvents
    )
import DomainDriven.Persistance.Postgres.Migration
import GHC.Generics (Generic)
import GHC.IO.Unsafe (unsafePerformIO)
import Streamly.Data.Stream.Prelude qualified as Stream
import Test.Hspec
import UnliftIO (concurrently, try)
import UnliftIO.Pool
import Prelude

eventTable :: EventTable
eventTable =
    MigrateUsing (\_ _ _ -> pure ())
        . MigrateUsing (\_ _ _ -> pure ())
        $ InitialVersion
            "test_events"

eventTable2 :: EventTable
eventTable2 = MigrateUsing mig eventTable
  where
    mig :: PreviousEventTableName -> EventTableName -> Connection -> IO ()
    mig prevName name conn = migrate1to1 @Value @_ @NoIndex conn prevName name id

spec :: Spec
spec = do
    aroundAll setupPersistance streamingSpec
    aroundAll setupPersistance $ do
        writeEventsSpec
        queryEventsSpec
        migrationSpec -- make sure migrationSpec is run last!
    around setupPersistance migrationConcurrencySpec
    around setupPersistanceIndexed indexedSpec

type TestModel = Int

data TestEvent
    = AddOne
    | SubtractOne
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

applyTestEvent :: TestModel -> Stored TestEvent -> TestModel
applyTestEvent m ev = case storedEvent ev of
    AddOne -> m + 1
    SubtractOne -> m - 1

setupPersistance
    :: ((PostgresEvent TestModel NoIndex TestEvent, Pool Connection) -> IO ())
    -> IO ()
setupPersistance test = do
    dropEventTables =<< mkTestConn
    let stripesAndResources = 5
    poolCfg <-
        setNumStripes (Just stripesAndResources)
            <$> mkDefaultPoolConfig mkTestConn close 1 stripesAndResources
    pool <- newPool poolCfg
    p <- postgresWriteModel pool eventTable applyTestEvent 0
    test (p{chunkSize = 2}, pool)

setupPersistanceIndexed
    :: ((PostgresEvent TestModel Indexed TestEvent, Pool Connection) -> IO ())
    -> IO ()
setupPersistanceIndexed test = do
    dropEventTables =<< mkTestConn
    let stripesAndResources = 5
    poolCfg <-
        setNumStripes (Just stripesAndResources)
            <$> mkDefaultPoolConfig mkTestConn close 1 stripesAndResources
    pool <- newPool poolCfg
    p <- postgresWriteModel pool eventTable applyTestEvent 0
    test (p{chunkSize = 2}, pool)

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

dropEventTables :: Connection -> IO ()
dropEventTables conn = do
    testTables <-
        query_
            conn
            "select table_name from information_schema.tables where table_name like 'test_events_v%'"
            :: IO [Only String]
    traverse_
        (\t -> execute_ conn ("drop table \"" <> fromString (fromOnly t) <> "\""))
        testTables

tableNames :: EventTable -> [EventTableName]
tableNames et = case et of
    MigrateUsing _ next -> getEventTableName et : tableNames next
    InitialVersion{} -> [getEventTableName et]

writeEventsSpec
    :: SpecWith (PostgresEvent TestModel NoIndex TestEvent, Pool Connection)
writeEventsSpec = describe "queryEvents" $ do
    let ev1 :: Stored TestEvent
        ev1 =
            Stored
                { storedEvent = AddOne
                , storedTimestamp = UTCTime (fromGregorian 2020 10 15) 0
                , storedUUID = nil
                }

    it "Can write event to database" $ \(_p, pool) -> withResource pool $ \conn -> do
        i <- writeEvents conn (getEventTableName eventTable) NoIndex [ev1]
        i `shouldBe` 1

    it "Writing the same event again fails" $ \(_p, pool) -> withResource pool $ \conn -> do
        writeEvents conn (getEventTableName eventTable) NoIndex [ev1]
            `shouldThrow` (== FatalError)
                . sqlExecStatus

    it "Writing multiple events at once works" $ \(p, pool) -> do
        let evs =
                [ AddOne
                , SubtractOne
                ]
        storedEvs <-
            traverse
                (\e -> Stored e (UTCTime (fromGregorian 2020 10 15) 10) <$> mkId)
                evs
        _ <- withResource pool $ \conn ->
            writeEvents conn (getEventTableName eventTable) NoIndex storedEvs
        evs' <- getEventList p NoIndex
        drop (length evs' - 2) (fmap storedEvent evs') `shouldBe` evs

indexedSpec :: SpecWith (PostgresEvent TestModel Indexed TestEvent, Pool Connection)
indexedSpec = describe "indexed" $ do
    it "Indexed models are updated separately" $ \(p, pool) -> do
        let evs1 = [AddOne, SubtractOne, AddOne]
            evs2 = [AddOne, AddOne, AddOne]

        storedEvs1 <-
            traverse
                (\e -> Stored e (UTCTime (fromGregorian 2020 10 15) 10) <$> mkId)
                evs1
        storedEvs2 <-
            traverse
                (\e -> Stored e (UTCTime (fromGregorian 2020 10 15) 10) <$> mkId)
                evs2
        _ <- withResource pool $ \conn ->
            writeEvents conn (getEventTableName eventTable) (Indexed "1") storedEvs1
        _ <- withResource pool $ \conn ->
            writeEvents conn (getEventTableName eventTable) (Indexed "2") storedEvs2
        m1 <- getModel p (Indexed "1")
        m2 <- getModel p (Indexed "2")
        m1 `shouldBe` 1
        m2 `shouldBe` 3

streamingSpec :: SpecWith (PostgresEvent TestModel NoIndex TestEvent, Pool Connection)
streamingSpec = describe "steaming" $ do
    it "getEventList and getEventStream yields the same result" $ \(p, pool) -> do
        storedEvs <- for ([1 .. 10] :: [Int]) $ \i -> do
            enKey <- mkId
            pure $ Stored AddOne (UTCTime (fromGregorian 2020 10 15) (fromIntegral i)) enKey
        _ <- withResource pool $ \conn ->
            writeEvents conn (getEventTableName eventTable) NoIndex storedEvs
        evList <- getEventList p NoIndex
        evStream <- Stream.toList $ getEventStream p NoIndex
        -- pPrint evList
        evList `shouldSatisfy` (== 10) . length -- must be at least two to verify order
        fmap storedEvent evStream `shouldBe` fmap storedEvent evList
        evStream `shouldBe` evList

queryEventsSpec
    :: SpecWith (PostgresEvent TestModel NoIndex TestEvent, Pool Connection)
queryEventsSpec = describe "queryEvents" $ do
    it "Can query events" $ \(_p, pool) -> withResource pool $ \conn -> do
        evs <- queryEvents @TestEvent conn (getEventTableName eventTable) NoIndex
        evs `shouldSatisfy` (>= 1) . length
    it "Events come out in the right order" $ \(_p, pool) -> withResource pool $ \conn -> do
        -- write few more events before
        --
        _ <- do
            id1 <- mkId
            let ev1 = SubtractOne
            _ <-
                writeEvents
                    conn
                    (getEventTableName eventTable)
                    NoIndex
                    [Stored ev1 (UTCTime (fromGregorian 2020 10 20) 1) id1]

            id2 <- mkId
            let ev2 = AddOne
            writeEvents
                conn
                (getEventTableName eventTable)
                NoIndex
                [Stored ev2 (UTCTime (fromGregorian 2020 10 18) 1) id2]

        evs <- queryEvents @TestEvent conn (getEventTableName eventTable) NoIndex
        evs `shouldSatisfy` (> 1) . length
        let event_numbers = fmap snd evs
        event_numbers `shouldSatisfy` (\n -> and $ zipWith (>) (drop 1 n) n)

migrationSpec :: SpecWith (PostgresEvent TestModel NoIndex TestEvent, Pool Connection)
migrationSpec = describe "migrate1to1" $ do
    it "Keeps all events when using `id` to update" $ \(_p, pool) -> do
        evs <- withResource pool $ \conn ->
            queryEvents @TestEvent conn (getEventTableName eventTable) NoIndex
        evs `shouldSatisfy` (>= 1) . length

        _ <- postgresWriteModel @NoIndex pool eventTable2 applyTestEvent 0
        evs' <- withResource pool $ \conn ->
            queryEvents @TestEvent conn (getEventTableName eventTable2) NoIndex

        fmap fst evs' `shouldBe` fmap fst evs

    it "Can no longer write new events to old table after migration" $ \(_p, pool) -> do
        uuid <- V4.nextRandom
        let ev =
                Stored
                    AddOne
                    (UTCTime (fromGregorian 2020 10 15) 0)
                    uuid
        withResource
            pool
            (\conn -> writeEvents conn (getEventTableName eventTable) NoIndex [ev])
            `shouldThrow` (== FatalError)
                . sqlExecStatus
    it "But can write to the new table" $ \(_p, pool) -> do
        uuid <- V4.nextRandom
        let ev =
                Stored
                    AddOne
                    (UTCTime (fromGregorian 2020 10 15) 0)
                    uuid

        void . withResource pool $ \conn -> writeEvents conn (getEventTableName eventTable2) NoIndex [ev]

    it "Broken migration throws and rollbacks transaction" $ \(_, pool) -> do
        let eventTableBroken :: EventTable
            eventTableBroken = MigrateUsing undefined eventTable2

        postgresWriteModel pool eventTableBroken applyTestEvent 0
            `shouldThrow` const @_ @SomeException True
        conn <- mkTestConn

        case tableNames eventTableBroken of
            failedMig : prevMig : _ -> do
                [Only prevExists] <-
                    query_ @(Only Bool) conn $
                        "select exists(select * from pg_tables where tablename='"
                            <> fromString prevMig
                            <> "')"
                [Only brokenExists] <-
                    query_ @(Only Bool) conn $
                        "select exists(select * from pg_tables where tablename='"
                            <> fromString failedMig
                            <> "')"
                prevExists `shouldBe` True
                brokenExists `shouldBe` False
            _ -> fail "Unexpectedly lacking table versions!"

migrationConcurrencySpec
    :: SpecWith (PostgresEvent TestModel NoIndex TestEvent, Pool Connection)
migrationConcurrencySpec = describe "Event table is locked during migration" $ do
    it "migrate1to1" $ \(m0, pool) -> migrationTest m0 pool mig1to1
    it "migrate1toMany" $ \(m0, pool) -> migrationTest m0 pool mig1toMany
    it "migrate1toManyWithState" $ \(m0, pool) -> migrationTest m0 pool mig1toManyState
  where
    migrationTest
        :: PostgresEvent TestModel NoIndex TestEvent
        -> Pool Connection
        -> EventMigration
        -> IO ()
    migrationTest m0 pool mig = do
        let cmd :: Int -> IO (Int -> Int, [TestEvent])
            cmd _ = pure (id, [AddOne])

        i <- replicateM 5 (transactionalUpdate m0 NoIndex cmd)
        length i `shouldBe` 5
        (result, _) <-
            concurrently
                ( do
                    threadDelay 100000 -- sleep a bit and let the migration start
                    try @IO @SqlError $ transactionalUpdate m0 NoIndex cmd
                )
                (postgresWriteModel pool (MigrateUsing mig eventTable2) applyTestEvent 0)
        result `shouldSatisfy` \case
            Right _ -> False
            Left err -> sqlErrorMsg err == "Event table has been retired."

    mig1to1 :: PreviousEventTableName -> EventTableName -> Connection -> IO ()
    mig1to1 prevName name conn = migrate1to1 @Value @_ @NoIndex conn prevName name slowId

    mig1toMany :: PreviousEventTableName -> EventTableName -> Connection -> IO ()
    mig1toMany prevName name conn = migrate1toMany @Value @_ @NoIndex conn prevName name (pure . slowId)

    mig1toManyState :: PreviousEventTableName -> EventTableName -> Connection -> IO ()
    mig1toManyState prevName name conn = do
        putStrLn "mig1toManyState"
        migrate1toManyWithState @Value @_ @_ @NoIndex
            conn
            prevName
            name
            (\s ev -> (s, [slowId ev]))
            ()
        putStrLn "mig1toManyState is done"

    slowId :: a -> a
    slowId a = unsafePerformIO $ do
        -- putStrLn "Migrating slowly..."
        threadDelay 250000
        pure a
