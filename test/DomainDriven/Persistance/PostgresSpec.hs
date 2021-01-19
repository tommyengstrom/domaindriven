{-# LANGUAGE OverloadedLists #-}
module DomainDriven.Persistance.PostgresSpec where

import           RIO
import           DomainDriven
import           DomainDriven.Persistance.Postgres
import           Test.Hspec
import           Database.PostgreSQL.Simple
import qualified StoreModel                                   as Store
import           Data.UUID                      ( nil )
import           RIO.Time
import qualified Data.Map                                     as M
import           Safe                           ( headNote )


spec :: Spec
spec = do
    conn <- runIO mkTestConn
    runIO $ do
        _ <- dropTables conn
        createTables p
    storeModelSpec
    writeEventsSpec conn
    writeStateSpec conn
    queryEventsSpec conn
    queryStateSpec conn
    clearStateSpec conn

mkTestConn :: IO Connection
mkTestConn = connect $ ConnectInfo { connectHost     = "localhost"
                                   , connectPort     = 5432
                                   , connectUser     = "postgres"
                                   , connectPassword = "test"
                                   , connectDatabase = "domaindriven"
                                   }


eventTable :: EventTableName
eventTable = "events"

stateTable :: StateTableName
stateTable = "state"

p :: PostgresStateAndEvent Store.StoreModel Store.StoreEvent
p = createPostgresPersistance mkTestConn
                              eventTable
                              stateTable
                              Store.applyStoreEvent
                              mempty

dropTables :: Connection -> IO Int64
dropTables conn =
    (+)
        <$> execute_ conn ("drop table if exists \"" <> fromString eventTable <> "\"")
        <*> execute_ conn ("drop table if exists \"" <> fromString stateTable <> "\"")

writeEventsSpec :: Connection -> Spec
writeEventsSpec conn = describe "queryEvents" $ do
    it "Can write event to database" $ do
        let
            ev = Stored (Store.RemovedItem $ Store.Wrap 1)
                        (UTCTime (fromGregorian 2020 10 15) 0)
                        nil
        i <- writeEvents p conn [ev]
        i `shouldBe` 1

    it "Writing the same event again fails" $ do
        let
            ev = Stored (Store.RemovedItem $ Store.Wrap 1)
                        (UTCTime (fromGregorian 2020 10 15) 0)
                        nil
        writeEvents p conn [ev] `shouldThrow` (== FatalError) . sqlExecStatus
    it "Writing multiple events at once works" $ do
        let evs =
                [ Store.AddedItem 10 (Store.ItemInfo { quantity = 23, price = 220 })
                , Store.BoughtItem 10 2
                ]
        storedEvs <- traverse
            (\e -> Stored e (UTCTime (fromGregorian 2020 10 15) 10) <$> mkId)
            evs
        i <- writeEvents p conn storedEvs
        i `shouldBe` 2



writeStateSpec :: Connection -> Spec
writeStateSpec conn = describe "writeState" $ do
    it "Can write state" $ do
        let s :: Store.StoreModel
            s = [(1, Store.ItemInfo 3 199)]
        i <- writeState p conn s
        i `shouldBe` 1
    it "Can overwrite state" $ do
        i <- writeState p conn mempty
        i `shouldBe` 1


queryEventsSpec :: Connection -> Spec
queryEventsSpec conn = describe "queryEvents" $ do
    it "Can query events" $ do
        evs <- queryEvents p conn
        evs `shouldSatisfy` (>= 1) . length
    it "Events come out in temporal order" $ do
        -- write few more events before
        --
        _ <- do
            id1 <- mkId
            let ev1 = Store.Restocked 1 4
            _ <- writeEvents p
                             conn
                             [Stored ev1 (UTCTime (fromGregorian 2020 10 20) 1) id1]

            id2 <- mkId
            let ev2 = Store.Restocked 1 10
            writeEvents p conn [Stored ev2 (UTCTime (fromGregorian 2020 10 18) 1) id2]

        evs <- queryEvents p conn
        evs `shouldSatisfy` (> 1) . length
        let timestamps = fmap storedTimestamp evs
        ("temporal order" :: Text)
            `shouldSatisfy` (const . and $ zipWith (<=) timestamps (drop 1 timestamps))


queryStateSpec :: Connection -> Spec
queryStateSpec conn = describe "queryState" $ do
    it "Can query state" $ do
        s <- queryState p conn
        s `shouldSatisfy` const True


clearStateSpec :: Connection -> Spec
clearStateSpec conn = describe "clearStateTable" $ do
    it "Clears all states" $ do
        s <- queryState p conn
        s `shouldSatisfy` (> 0) . length
        i <- clearStateTable p conn
        fromIntegral i `shouldBe` length s


storeModelSpec :: Spec
storeModelSpec = describe "Test basic functionality" $ do
    it "Can add item" $ do
        let item :: Store.ItemInfo
            item = Store.ItemInfo 10 49
        iKey <- runCmd p Store.handleStoreCmd $ Store.AddItem item
        getModel p `shouldReturn` M.singleton iKey item

    it "Can buy item" $ do
        iKey <- headNote "Ops" . M.keys <$> getModel p
        runCmd p Store.handleStoreCmd $ Store.BuyItem iKey 7
        getModel p `shouldReturn` M.singleton (Store.Wrap 1) (Store.ItemInfo 3 49)


    it "Can run Query" $ do
        runQuery p Store.queryHandler Store.ProductCount `shouldReturn` 1

        let item :: Store.ItemInfo
            item = Store.ItemInfo 4 33
        _ <- runCmd p Store.handleStoreCmd $ Store.AddItem item

        runQuery p Store.queryHandler Store.ProductCount `shouldReturn` 2
