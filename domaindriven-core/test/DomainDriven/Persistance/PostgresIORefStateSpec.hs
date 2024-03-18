module DomainDriven.Persistance.PostgresIORefStateSpec where

import Control.Exception (SomeException)
import Control.Monad
import Data.Aeson (FromJSON, ToJSON, Value)
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
import Streamly.Data.Stream.Prelude qualified as Stream
import Test.Hspec
import UnliftIO.Pool
import Prelude

-- import           Text.Pretty.Simple

eventTable :: EventTable
eventTable =
    MigrateUsing (\_ _ _ -> pure ()) . MigrateUsing (\_ _ _ -> pure ()) $
        InitialVersion
            "test_events"

eventTable2 :: EventTable
eventTable2 = MigrateUsing mig eventTable
  where
    mig :: PreviousEventTableName -> EventTableName -> Connection -> IO ()
    mig prevName name conn = migrate1to1 @Value conn prevName name id

spec :: Spec
spec = do
    aroundAll setupPersistance streamingSpec
    aroundAll setupPersistance $ do
        writeEventsSpec
        queryEventsSpec
        migrationSpec -- make sure migrationSpec is run last!

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
    :: ((PostgresEvent TestModel TestEvent, Pool Connection) -> IO ())
    -> IO ()
setupPersistance test = do
    dropEventTables =<< mkTestConn
    poolCfg <- mkDefaultPoolConfig (mkTestConn) close 1 5
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
dropEventTables conn = void . for (tableNames eventTable2) $ \n ->
    void $ execute_ conn ("drop table if exists \"" <> fromString n <> "\"")

tableNames :: EventTable -> [EventTableName]
tableNames et = case et of
    MigrateUsing _ next -> getEventTableName et : tableNames next
    InitialVersion{} -> [getEventTableName et]

withPool :: (Pool Connection -> IO a) -> IO a
withPool f = do
    poolCfg <- mkDefaultPoolConfig (mkTestConn) close 1 5
    pool <- newPool poolCfg
    r <- f pool
    destroyAllResources pool
    pure r

writeEventsSpec :: SpecWith (PostgresEvent TestModel TestEvent, Pool Connection)
writeEventsSpec = describe "queryEvents" $ do
    let ev1 :: Stored TestEvent
        ev1 =
            Stored
                { storedEvent = AddOne
                , storedTimestamp = UTCTime (fromGregorian 2020 10 15) 0
                , storedUUID = nil
                }

    it "Can write event to database" $ \(_p, pool) -> withResource pool $ \conn -> do
        i <- writeEvents conn (getEventTableName eventTable) [ev1]
        i `shouldBe` 1

    it "Writing the same event again fails" $ \(_p, pool) -> withResource pool $ \conn -> do
        writeEvents conn (getEventTableName eventTable) [ev1]
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
            writeEvents conn (getEventTableName eventTable) storedEvs
        evs' <- getEventList p
        drop (length evs' - 2) (fmap storedEvent evs') `shouldBe` evs

streamingSpec :: SpecWith (PostgresEvent TestModel TestEvent, Pool Connection)
streamingSpec = describe "steaming" $ do
    it "getEventList and getEventStream yields the same result" $ \(p, pool) -> do
        storedEvs <- for ([1 .. 10] :: [Int]) $ \i -> do
            enKey <- mkId
            pure $ Stored AddOne (UTCTime (fromGregorian 2020 10 15) (fromIntegral i)) enKey
        _ <- withResource pool $ \conn ->
            writeEvents conn (getEventTableName eventTable) storedEvs
        evList <- getEventList p
        evStream <- Stream.toList $ getEventStream p
        -- pPrint evList
        evList `shouldSatisfy` (== 10) . length -- must be at least two to verify order
        fmap storedEvent evStream `shouldBe` fmap storedEvent evList
        evStream `shouldBe` evList

queryEventsSpec :: SpecWith (PostgresEvent TestModel TestEvent, Pool Connection)
queryEventsSpec = describe "queryEvents" $ do
    it "Can query events" $ \(_p, pool) -> withResource pool $ \conn -> do
        evs <- queryEvents @TestEvent conn (getEventTableName eventTable)
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
                    [Stored ev1 (UTCTime (fromGregorian 2020 10 20) 1) id1]

            id2 <- mkId
            let ev2 = AddOne
            writeEvents
                conn
                (getEventTableName eventTable)
                [Stored ev2 (UTCTime (fromGregorian 2020 10 18) 1) id2]

        evs <- queryEvents @TestEvent conn (getEventTableName eventTable)
        evs `shouldSatisfy` (> 1) . length
        let event_numbers = fmap snd evs
        event_numbers `shouldSatisfy` (\n -> and $ zipWith (>) (drop 1 n) n)

migrationSpec :: SpecWith (PostgresEvent TestModel TestEvent, Pool Connection)
migrationSpec = describe "migrate1to1" $ do
    it "Keeps all events when using `id` to update" $ \(_p, pool) -> do
        evs <- withResource pool $ \conn ->
            queryEvents @TestEvent conn (getEventTableName eventTable)
        evs `shouldSatisfy` (>= 1) . length

        _ <- postgresWriteModel pool eventTable2 applyTestEvent 0
        evs' <- withResource pool $ \conn ->
            queryEvents @TestEvent conn (getEventTableName eventTable2)

        fmap fst evs' `shouldBe` fmap fst evs
    it "Can no longer write new events to old table after migration" $ \(_p, pool) -> do
        uuid <- V4.nextRandom
        let ev =
                Stored
                    AddOne
                    (UTCTime (fromGregorian 2020 10 15) 0)
                    uuid
        withResource pool (\conn -> writeEvents conn (getEventTableName eventTable) [ev])
            `shouldThrow` (== FatalError)
                . sqlExecStatus
    it "But can write to the new table" $ \(_p, pool) -> do
        uuid <- V4.nextRandom
        let ev =
                Stored
                    AddOne
                    (UTCTime (fromGregorian 2020 10 15) 0)
                    uuid

        void . withResource pool $ \conn -> writeEvents conn (getEventTableName eventTable2) [ev]

    it "Broken migration throws and rollbacks transaction" $ \(_, pool) -> do
        let eventTable3 :: EventTable
            eventTable3 = MigrateUsing undefined eventTable2

        print $ tableNames eventTable3

        postgresWriteModel pool eventTable3 applyTestEvent 0
            `shouldThrow` const @_ @SomeException True
        conn <- mkTestConn

        case tableNames eventTable3 of
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
