module DomainDriven.Persistance.Postgres.TypesSpec where

import Control.Exception (displayException)
import Data.Foldable (for_)
import DomainDriven.Persistance.Postgres.Internal
    ( getEventTableName
    , validateEventTable
    , validateEventTableName
    )
import DomainDriven.Persistance.Postgres.Types
    ( EventMigration
    , EventTable (..)
    , MigrationError (..)
    , quoteIdent
    )
import Test.Hspec
import Prelude

noop :: EventMigration
noop _ _ _ = pure ()

spec :: Spec
spec = do
    describe "quoteIdent" $ do
        it "quotes a simple identifier" $ do
            quoteIdent "foo" `shouldBe` "\"foo\""
        it "escapes embedded double quotes by doubling them" $ do
            quoteIdent "has\"quote" `shouldBe` "\"has\"\"quote\""
        it "handles empty string" $ do
            quoteIdent "" `shouldBe` "\"\""

    describe "getEventTableName" $ do
        it "computes name for TableName" $ do
            getEventTableName (TableName "valid_name" 1) `shouldBe` "valid_name_v1"
        it "starts counting at the TableName version" $ do
            getEventTableName (TableName "tbl" 48) `shouldBe` "tbl_v48"
        it "names the table after the newest MigrateTo" $ do
            getEventTableName (MigrateTo 50 noop . MigrateTo 49 noop $ TableName "tbl" 48)
                `shouldBe` "tbl_v50"
        it "renders names independently of validation" $ do
            getEventTableName (TableName "bad;name" 1) `shouldBe` "bad;name_v1"
            getEventTableName (TableName (replicate 61 'a') 1)
                `shouldBe` replicate 61 'a' <> "_v1"

    describe "validateEventTableName" $ do
        it "rejects empty and unsafe raw names" $ do
            for_ ["", "bad;name", "bad\"name", "bad\0name", "é"] $ \name ->
                validateEventTableName name `shouldThrow` (== InvalidEventTableName name)
        it "accepts 63-character names" $ do
            validateEventTableName (replicate 63 'a') `shouldReturn` ()
        it "rejects 64-character names" $ do
            validateEventTableName (replicate 64 'a')
                `shouldThrow` (== EventTableNameTooLong (replicate 64 'a'))

    describe "validateEventTable" $ do
        it "accepts a well-formed chain" $ do
            validateEventTable (MigrateTo 50 noop . MigrateTo 49 noop $ TableName "events" 48)
                `shouldReturn` ()
        it "rejects invalid base names" $ do
            validateEventTable (TableName "bad name" 1)
                `shouldThrow` (== InvalidEventTableBaseName "bad name")
            validateEventTable (TableName "" 1)
                `shouldThrow` (== InvalidEventTableBaseName "")
        it "rejects versions below 1" $ do
            validateEventTable (TableName "events" 0)
                `shouldThrow` (== InvalidEventTableVersion "events" 0)
        it "accepts a current table name of 63 characters" $ do
            validateEventTable (TableName (replicate 60 'a') 9) `shouldReturn` ()
        it "rejects a chain whose newest step pushes the name past 63 characters" $ do
            validateEventTable (MigrateTo 10 noop $ TableName (replicate 60 'a') 9)
                `shouldThrow` (== EventTableNameTooLong (replicate 60 'a' <> "_v10"))
            displayException (EventTableNameTooLong (replicate 60 'a' <> "_v10"))
                `shouldContain` "is 64 characters long; the limit is 63"
        it "rejects a first step that is not one above the TableName version" $ do
            validateEventTable (MigrateTo 50 noop $ TableName "events" 48)
                `shouldThrow` (== MisnumberedMigration "events" 49 50)
            validateEventTable (MigrateTo 48 noop $ TableName "events" 48)
                `shouldThrow` (== MisnumberedMigration "events" 49 48)
        it "rejects a gap between steps" $ do
            validateEventTable (MigrateTo 51 noop . MigrateTo 49 noop $ TableName "events" 48)
                `shouldThrow` (== MisnumberedMigration "events" 50 51)
        it "rejects a repeated step" $ do
            validateEventTable (MigrateTo 49 noop . MigrateTo 49 noop $ TableName "events" 48)
                `shouldThrow` (== MisnumberedMigration "events" 50 49)
        it "rejects steps listed oldest first" $ do
            validateEventTable (MigrateTo 49 noop . MigrateTo 50 noop $ TableName "events" 48)
                `shouldThrow` (== MisnumberedMigration "events" 49 50)
            displayException (MisnumberedMigration "events" 49 50)
                `shouldContain` "found MigrateTo 50 where MigrateTo 49 was expected. The chain is written newest first"
