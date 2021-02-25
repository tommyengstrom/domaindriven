{-# LANGUAGE OverloadedLists #-}
module DomainDriven.Persistance.PostgresSpec where

import           Prelude
import           DomainDriven
import qualified Data.Text                                    as T
import           DomainDriven.Persistance.Postgres
import           Test.Hspec
import           Database.PostgreSQL.Simple
import qualified StoreModel                                   as Store
import           Data.UUID                      ( nil )
import           Data.Time
import           Data.Int                       ( Int64 )
import           Data.String                    ( fromString )
import           Data.Text                      ( Text )
import qualified Data.Map                                     as M
import           Safe                           ( headNote )
import           Control.Concurrent.Async


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
                                   , connectPassword = "postgres"
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
        let ev = Stored (Store.RemovedItem $ Store.ItemKey nil)
                        (UTCTime (fromGregorian 2020 10 15) 0)
                        nil
        i <- writeEvents p conn [ev]
        i `shouldBe` 1

    it "Writing the same event again fails" $ do
        let ev = Stored (Store.RemovedItem $ Store.ItemKey nil)
                        (UTCTime (fromGregorian 2020 10 15) 0)
                        nil
        writeEvents p conn [ev] `shouldThrow` (== FatalError) . sqlExecStatus
--    it "Writing multiple events at once works" $ do
--        let evs =
--                [ Store.AddedItem 10 (Store.ItemInfo { quantity = 23, price = 220 })
--                , Store.BoughtItem 10 2
--                ]
--        storedEvs <- traverse
--            (\e -> Stored e (UTCTime (fromGregorian 2020 10 15) 10) <$> mkId)
--            evs
--        i <- writeEvents p conn storedEvs
--        i `shouldBe` 2



writeStateSpec :: Connection -> Spec
writeStateSpec conn = describe "writeState" $ do
    it "Can write state" $ do
        let s :: Store.StoreModel
            s =
                [ ( Store.ItemKey nil
                  , Store.ItemInfo (Store.ItemKey nil) "test item" 3 199
                  )
                ]
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
            let ev1 = Store.Restocked (Store.ItemKey nil) 4
            _ <- writeEvents p
                             conn
                             [Stored ev1 (UTCTime (fromGregorian 2020 10 20) 1) id1]

            id2 <- mkId
            let ev2 = Store.Restocked (Store.ItemKey nil) 10
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
        iKey <- runCmd p Store.handleStoreAction $ Store.AdminAction $ Store.AddItem
            "test item"
            10
            99
        m    <- getModel p
        item <- maybe (fail $ show iKey <> " is not part of:\n" <> show m) pure
            $ M.lookup iKey m
        Store.price item `shouldBe` 99
        Store.quantity item `shouldBe` 10

    it "Can buy item" $ do
        iKey <- headNote "Ops" . M.keys <$> getModel p
        runCmd p Store.handleStoreAction $ Store.BuyItem iKey 7
        m    <- getModel p
        item <- maybe (fail $ show iKey <> " is not part of:\n" <> show m) pure
            $ M.lookup iKey m
        Store.price item `shouldBe` 99
        Store.quantity item `shouldBe` 3


    it "Can run Query" $ do
        items <- runCmd p Store.handleStoreAction Store.ListItems
        items `shouldSatisfy` (== 1) . length

        _ <- runCmd p Store.handleStoreAction $ Store.AdminAction $ Store.AddItem
            "another item"
            1
            10

        items' <- runCmd p Store.handleStoreAction Store.ListItems
        items' `shouldSatisfy` (== 2) . length
    it "Concurrent commands work" $ do
        -- This test relies on the postgres max connections being reasonably high.
        c0 <- runCmd p Store.handleStoreAction Store.ListItems
        let newItems :: [Store.StoreAction CMD Store.ItemKey]
            newItems = replicate
                n
                ( Store.AdminAction
                $ Store.AddItem (Store.ItemName $ "item_" <> T.pack (show n)) 5 50
                )

            n :: Int
            n = 30
        mapConcurrently_ (runCmd p Store.handleStoreAction) newItems

        c1 <- runCmd p Store.handleStoreAction Store.ListItems
        (length c1, length c0)
            `shouldSatisfy` (\(after', before') -> after' - before' == n)
