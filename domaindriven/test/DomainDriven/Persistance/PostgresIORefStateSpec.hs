{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module DomainDriven.Persistance.PostgresIORefStateSpec where

import Control.Concurrent.Async
import Control.Exception (SomeException)
import Control.Monad
import Data.Aeson
    ( Value
    , encode
    )
import Data.Generics.Product
import qualified Data.Map as M
import Data.String (fromString)
import qualified Data.Text as T
import Data.Time
import Data.Traversable
import Data.UUID (nil)
import qualified Data.UUID.V4 as V4
import Database.PostgreSQL.Simple
import DomainDriven
import DomainDriven.Persistance.Postgres
import DomainDriven.Persistance.Postgres.Internal
    ( getEventTableName
    , queryEvents
    , writeEvents
    )
import DomainDriven.Persistance.Postgres.Migration
import Lens.Micro ((^.))
import qualified Models.Store as Store
import qualified Streamly.Prelude as S
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
    aroundAll setupPersistance $ storeModelSpec
    aroundAll setupPersistance $ do
        streaming
    aroundAll setupPersistance $ do
        writeEventsSpec
        queryEventsSpec
        -- make sure migrationSpec is run last!
        migrationSpec

setupPersistance
    :: ((PostgresEvent Store.StoreModel Store.StoreEvent, Pool Connection) -> IO ())
    -> IO ()
setupPersistance test = do
    dropEventTables =<< mkTestConn
    pool <- createPool (mkTestConn) close 1 5 1
    p <- postgresWriteModel pool eventTable Store.applyStoreEvent mempty
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
    pool <- createPool (mkTestConn) close 1 5 1
    r <- f pool
    destroyAllResources pool
    pure r

writeEventsSpec :: SpecWith (PostgresEvent Store.StoreModel Store.StoreEvent, Pool Connection)
writeEventsSpec = describe "queryEvents" $ do
    it "Can write event to database" $ \(_p, pool) -> withResource pool $ \conn -> do
        let ev =
                Stored
                    (Store.RemovedItem $ Store.ItemKey nil)
                    (UTCTime (fromGregorian 2020 10 15) 0)
                    nil
        i <- writeEvents conn (getEventTableName eventTable) [ev]
        i `shouldBe` 1

    it "Writing the same event again fails" $ \(_p, pool) -> withResource pool $ \conn -> do
        let ev =
                Stored
                    (Store.RemovedItem $ Store.ItemKey nil)
                    (UTCTime (fromGregorian 2020 10 15) 0)
                    nil
        writeEvents conn (getEventTableName eventTable) [ev]
            `shouldThrow` (== FatalError)
                . sqlExecStatus
    it "Writing multiple events at once works" $ \(p, pool) -> do
        let evs =
                [ Store.AddedItem (Store.ItemKey nil) "Test item" 220
                , Store.BoughtItem (Store.ItemKey nil) 2
                ]
        storedEvs <-
            traverse
                (\e -> Stored e (UTCTime (fromGregorian 2020 10 15) 10) <$> mkId)
                evs
        _ <- withResource pool $ \conn ->
            writeEvents conn (getEventTableName eventTable) storedEvs
        evs' <- getEventList p
        drop (length evs' - 2) (fmap storedEvent evs') `shouldBe` evs

streaming :: SpecWith (PostgresEvent Store.StoreModel Store.StoreEvent, Pool Connection)
streaming = describe "steaming" $ do
    it "getEventList and getEventStream yields the same result" $ \(p, pool) -> do
        storedEvs <- for ([1 .. 10] :: [Int]) $ \i -> do
            enKey <- mkId
            let e = Store.BoughtItem (Store.ItemKey nil) (Store.Quantity i)
            pure $ Stored e (UTCTime (fromGregorian 2020 10 15) (fromIntegral i)) enKey
        _ <- withResource pool $ \conn ->
            writeEvents conn (getEventTableName eventTable) storedEvs
        evList <- getEventList p
        evStream <- S.toList $ getEventStream p
        -- pPrint evList
        evList `shouldSatisfy` (== 10) . length -- must be at least two to verify order
        fmap storedEvent evStream `shouldBe` fmap storedEvent evList
        evStream `shouldBe` evList

queryEventsSpec :: SpecWith (PostgresEvent Store.StoreModel Store.StoreEvent, Pool Connection)
queryEventsSpec = describe "queryEvents" $ do
    it "Can query events" $ \(_p, pool) -> withResource pool $ \conn -> do
        evs <- queryEvents @Store.StoreEvent conn (getEventTableName eventTable)
        evs `shouldSatisfy` (>= 1) . length
    it "Events come out in the right order" $ \(_p, pool) -> withResource pool $ \conn -> do
        -- write few more events before
        --
        _ <- do
            id1 <- mkId
            let ev1 = Store.Restocked (Store.ItemKey nil) 4
            _ <-
                writeEvents
                    conn
                    (getEventTableName eventTable)
                    [Stored ev1 (UTCTime (fromGregorian 2020 10 20) 1) id1]

            id2 <- mkId
            let ev2 = Store.Restocked (Store.ItemKey nil) 10
            writeEvents
                conn
                (getEventTableName eventTable)
                [Stored ev2 (UTCTime (fromGregorian 2020 10 18) 1) id2]

        evs <- queryEvents @Store.StoreEvent conn (getEventTableName eventTable)
        evs `shouldSatisfy` (> 1) . length
        let event_numbers = fmap snd evs
        event_numbers `shouldSatisfy` (\n -> and $ zipWith (>) (drop 1 n) n)

migrationSpec :: SpecWith (PostgresEvent Store.StoreModel Store.StoreEvent, Pool Connection)
migrationSpec = describe "migrate1to1" $ do
    it "Keeps all events when using `id` to update" $ \(_p, pool) -> do
        evs <- withResource pool $ \conn ->
            queryEvents @Store.StoreEvent conn (getEventTableName eventTable)
        evs `shouldSatisfy` (>= 1) . length

        _ <- postgresWriteModel pool eventTable2 Store.applyStoreEvent mempty
        evs' <- withResource pool $ \conn ->
            queryEvents @Store.StoreEvent conn (getEventTableName eventTable2)

        fmap fst evs' `shouldBe` fmap fst evs
    it "Can no longer write new events to old table after migration" $ \(_p, pool) -> do
        uuid <- V4.nextRandom
        let ev =
                Stored
                    (Store.AddedItem (Store.ItemKey uuid) "Test item" 220)
                    (UTCTime (fromGregorian 2020 10 15) 0)
                    uuid
        withResource pool (\conn -> writeEvents conn (getEventTableName eventTable) [ev])
            `shouldThrow` (== FatalError)
                . sqlExecStatus
    it "But can write to the new table" $ \(_p, pool) -> do
        uuid <- V4.nextRandom
        let ev =
                Stored
                    (Store.AddedItem (Store.ItemKey uuid) "Test item" 220)
                    (UTCTime (fromGregorian 2020 10 15) 0)
                    uuid

        void . withResource pool $ \conn -> writeEvents conn (getEventTableName eventTable2) [ev]

    it "Broken migration throws and rollbacks transaction" $ \(_, pool) -> do
        let eventTable3 :: EventTable
            eventTable3 = MigrateUsing undefined eventTable2

        print $ tableNames eventTable3

        postgresWriteModel pool eventTable3 Store.applyStoreEvent mempty
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

storeModelSpec :: SpecWith (PostgresEvent Store.StoreModel Store.StoreEvent, Pool Connection)
storeModelSpec = describe "Test basic functionality" $ do
    it "Can add item" $ \(p, _pool) -> do
        iKey <-
            runAction p Store.handleStoreAction $
                Store.AdminAction $
                    Store.AddItem
                        "test item"
                        10
                        99
        m <- getModel p
        item <-
            maybe (fail $ show iKey <> " is not part of:\n" <> show m) pure $
                M.lookup iKey m
        Store.price item `shouldBe` 99
        Store.quantity item `shouldBe` 10

    it "Can buy item" $ \(p, _pool) -> do
        iKey <- head . M.keys <$> getModel p
        runAction p Store.handleStoreAction $ Store.ItemAction iKey $ Store.ItemBuy 7
        m <- getModel p
        item <-
            maybe (fail $ show iKey <> " is not part of:\n" <> show m) pure $
                M.lookup iKey m
        Store.price item `shouldBe` 99
        Store.quantity item `shouldBe` 3

    it "Can run Query" $ \(p, _pool) -> do
        items <- runAction p Store.handleStoreAction Store.ListItems
        items `shouldSatisfy` (== 1) . length

        _ <-
            runAction p Store.handleStoreAction $
                Store.AdminAction $
                    Store.AddItem
                        "another item"
                        1
                        10

        items' <- runAction p Store.handleStoreAction Store.ListItems
        items' `shouldSatisfy` (== 2) . length
    it "Concurrent commands work" $ \(p, _pool) -> do
        -- This test relies on the postgres max connections being reasonably high.
        c0 <- runAction p Store.handleStoreAction Store.ListItems
        let newItems :: [Store.StoreAction 'ParamType Cmd Store.ItemKey]
            newItems =
                replicate
                    n
                    ( Store.AdminAction $
                        Store.AddItem (Store.ItemName $ "item_" <> T.pack (show n)) 5 50
                    )

            n :: Int
            n = 30
        mapConcurrently_ (runAction p Store.handleStoreAction) newItems

        c1 <- runAction p Store.handleStoreAction Store.ListItems
        (length c1, length c0)
            `shouldSatisfy` (\(after', before') -> after' - before' == n)
    it "Running a command where there are unevaliated events" $ \(p, _pool) -> do
        conn <- mkTestConn
        key1 <- mkId
        key2 <- mkId
        let iKey = Store.ItemKey nil
        now <- getCurrentTime
        _ <-
            executeMany
                conn
                ( "insert into \""
                    <> fromString (p ^. field @"eventTableName")
                    <> "\" (id, timestamp, event) \
                       \values (?, ?, ?)"
                )
                [
                    ( key1
                    , now
                    , encode $
                        Store.AddedItem iKey (Store.ItemName "test item") (Store.Price 10)
                    )
                , (key2, now, encode $ Store.Restocked iKey (Store.Quantity 10))
                ]
        runAction p Store.handleStoreAction $ Store.ItemAction iKey (Store.ItemBuy 1)
