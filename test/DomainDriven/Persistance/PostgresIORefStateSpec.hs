{-# LANGUAGE OverloadedLists #-}
module DomainDriven.Persistance.PostgresIORefStateSpec where

import           Prelude
import           DomainDriven
import qualified Data.Text                                    as T
import           DomainDriven.Persistance.PostgresIORefState
import           Test.Hspec
import           Test.Hspec.Core.Hooks
import           Database.PostgreSQL.Simple
import qualified StoreModel                                   as Store
import           Control.Monad
import           Data.UUID                      ( nil )
import           Data.Time
import           Data.String                    ( fromString )
import qualified Data.Map                                     as M
import           Safe                           ( headNote )
import           Control.Concurrent.Async


spec :: Spec
spec = do
    aroundAll setupPersistance $ storeModelSpec
    aroundAll setupPersistance $ do
        writeEventsSpec
        queryEventsSpec
  where
    setupPersistance
        :: (PostgresEvent Store.StoreModel Store.StoreEvent -> IO ()) -> IO ()
    setupPersistance test = do
        dropEventTable =<< mkTestConn
        p <- simplePostgres mkTestConn eventTable Store.applyStoreEvent mempty
        createEventTable p
        test p

mkTestConn :: IO Connection
mkTestConn = connect $ ConnectInfo { connectHost     = "localhost"
                                   , connectPort     = 5432
                                   , connectUser     = "postgres"
                                   , connectPassword = "postgres"
                                   , connectDatabase = "domaindriven"
                                   }


eventTable :: EventTableName
eventTable = "test_events"

dropEventTable :: Connection -> IO ()
dropEventTable conn =
    void $ execute_ conn ("drop table if exists \"" <> fromString eventTable <> "\"")

writeEventsSpec :: SpecWith (PostgresEvent Store.StoreModel Store.StoreEvent)
writeEventsSpec = describe "queryEvents" $ do
    it "Can write event to database" $ \p -> do
        let ev = Stored (Store.RemovedItem $ Store.ItemKey nil)
                        (UTCTime (fromGregorian 2020 10 15) 0)
                        nil
        conn <- getConnection p
        i    <- writeEvents conn eventTable [ev]
        i `shouldBe` 1

    it "Writing the same event again fails" $ \p -> do
        let ev = Stored (Store.RemovedItem $ Store.ItemKey nil)
                        (UTCTime (fromGregorian 2020 10 15) 0)
                        nil
        conn <- getConnection p
        writeEvents conn eventTable [ev] `shouldThrow` (== FatalError) . sqlExecStatus
    it "Writing multiple events at once works" $ \p -> do
        let evs =
                [ Store.AddedItem (Store.ItemKey nil) "Test item" 220
                , Store.BoughtItem (Store.ItemKey nil) 2
                ]
        storedEvs <- traverse
            (\e -> Stored e (UTCTime (fromGregorian 2020 10 15) 10) <$> mkId)
            evs
        conn <- getConnection p
        _    <- writeEvents conn eventTable storedEvs
        evs' <- getEvents p
        drop (length evs' - 2) (fmap storedEvent evs') `shouldBe` evs



queryEventsSpec :: SpecWith (PostgresEvent Store.StoreModel Store.StoreEvent)
queryEventsSpec = describe "queryEvents" $ do
    it "Can query events" $ \p -> do
        conn <- getConnection p
        evs  <- queryEvents @IO @Store.StoreEvent conn eventTable
        evs `shouldSatisfy` (>= 1) . length
    it "Events come out in the right order" $ \p -> do
        -- write few more events before
        --
        conn <- getConnection p
        _    <- do
            id1 <- mkId
            let ev1 = Store.Restocked (Store.ItemKey nil) 4
            _ <- writeEvents conn
                             eventTable
                             [Stored ev1 (UTCTime (fromGregorian 2020 10 20) 1) id1]

            id2 <- mkId
            let ev2 = Store.Restocked (Store.ItemKey nil) 10
            writeEvents conn
                        eventTable
                        [Stored ev2 (UTCTime (fromGregorian 2020 10 18) 1) id2]

        evs <- queryEvents @IO @Store.StoreEvent conn eventTable
        evs `shouldSatisfy` (> 1) . length
        let event_numbers = fmap snd evs
        event_numbers `shouldSatisfy` (\n -> and $ zipWith (>) (drop 1 n) n)




storeModelSpec :: SpecWith (PostgresEvent Store.StoreModel Store.StoreEvent)
storeModelSpec = describe "Test basic functionality" $ do
    it "Can add item" $ \p -> do
        iKey <- runAction p Store.handleStoreAction $ Store.AdminAction $ Store.AddItem
            "test item"
            10
            99
        m    <- getModel p
        item <- maybe (fail $ show iKey <> " is not part of:\n" <> show m) pure
            $ M.lookup iKey m
        Store.price item `shouldBe` 99
        Store.quantity item `shouldBe` 10

    it "Can buy item" $ \p -> do
        iKey <- headNote "Ops" . M.keys <$> getModel p
        runAction p Store.handleStoreAction $ Store.BuyItem iKey 7
        m    <- getModel p
        item <- maybe (fail $ show iKey <> " is not part of:\n" <> show m) pure
            $ M.lookup iKey m
        Store.price item `shouldBe` 99
        Store.quantity item `shouldBe` 3


    it "Can run Query" $ \p -> do
        items <- runAction p Store.handleStoreAction Store.ListItems
        items `shouldSatisfy` (== 1) . length

        _ <- runAction p Store.handleStoreAction $ Store.AdminAction $ Store.AddItem
            "another item"
            1
            10

        items' <- runAction p Store.handleStoreAction Store.ListItems
        items' `shouldSatisfy` (== 2) . length
    it "Concurrent commands work" $ \p -> do
        -- This test relies on the postgres max connections being reasonably high.
        c0 <- runAction p Store.handleStoreAction Store.ListItems
        let newItems :: [Store.StoreAction CMD Store.ItemKey]
            newItems = replicate
                n
                ( Store.AdminAction
                $ Store.AddItem (Store.ItemName $ "item_" <> T.pack (show n)) 5 50
                )

            n :: Int
            n = 30
        mapConcurrently_ (runAction p Store.handleStoreAction) newItems

        c1 <- runAction p Store.handleStoreAction Store.ListItems
        (length c1, length c0)
            `shouldSatisfy` (\(after', before') -> after' - before' == n)
