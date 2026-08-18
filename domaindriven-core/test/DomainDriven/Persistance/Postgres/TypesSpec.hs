module DomainDriven.Persistance.Postgres.TypesSpec where

import Control.Exception (ErrorCall, evaluate)
import Data.Map.Strict qualified as M
import DomainDriven.Persistance.Postgres.Internal
    ( findTagShift
    , getEventTableName
    , validateEventTable
    )
import DomainDriven.Persistance.Postgres.Types
    ( EventMigration
    , EventTable (..)
    , MigrationError (..)
    , MigrationOrigin (..)
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
        it "adds one version per MigrateWith" $ do
            getEventTableName
                (MigrateWith "b" noop . MigrateWith "a" noop $ TableName "tbl" 48)
                `shouldBe` "tbl_v50"
        it "names tables like the pre-0.7 chain did (TableName base 1 + k wrappers = base_v(k+1))" $ do
            let chain k = foldr (\i -> MigrateWith ("m" <> show i) noop) (TableName "b" 1) [1 .. k :: Int]
            map (getEventTableName . chain) [0 .. 4]
                `shouldBe` ["b_v1", "b_v2", "b_v3", "b_v4", "b_v5"]
        it "rejects names with unsafe characters" $ do
            evaluate (getEventTableName (TableName "bad;name" 1))
                `shouldThrow` \(_ :: ErrorCall) -> True
        it "rejects names with double quotes" $ do
            evaluate (getEventTableName (TableName "bad\"name" 1))
                `shouldThrow` \(_ :: ErrorCall) -> True

    describe "validateEventTable" $ do
        it "accepts a well-formed chain" $ do
            validateEventTable (MigrateWith "b" noop . MigrateWith "a" noop $ TableName "events" 48)
                `shouldReturn` ()
        it "rejects invalid base names" $ do
            validateEventTable (TableName "bad name" 1)
                `shouldThrow` (== InvalidEventTableBaseName "bad name")
            validateEventTable (TableName "" 1)
                `shouldThrow` (== InvalidEventTableBaseName "")
        it "rejects versions below 1" $ do
            validateEventTable (TableName "events" 0)
                `shouldThrow` (== InvalidEventTableVersion "events" 0)
        it "rejects empty tags" $ do
            validateEventTable (MigrateWith "" noop $ TableName "events" 3)
                `shouldThrow` (== InvalidMigrationTag "events" 4 "")
        it "rejects duplicate tags within the chain" $ do
            validateEventTable
                ( MigrateWith "same" noop . MigrateWith "other" noop . MigrateWith "same" noop $
                    TableName "events" 1
                )
                `shouldThrow` (== DuplicateMigrationTag "events" "same" [2, 4])

    describe "findTagShift" $ do
        let recorded =
                M.fromList [(49, ("add-email", OriginMigration)), (50, ("split-name", OriginMigration))]
        it "finds the code's tags one version later in the recorded history" $ do
            findTagShift [(48, "add-email"), (49, "split-name")] recorded `shouldBe` Just 1
        it "finds the code's tags one version earlier in the recorded history" $ do
            findTagShift [(50, "add-email"), (51, "split-name")] recorded `shouldBe` Just (-1)
        it "reports no shift when the sequences align" $ do
            findTagShift [(49, "add-email"), (50, "split-name")] recorded `shouldBe` Nothing
        it "reports no shift when no offset aligns (a migration inserted mid-chain)" $ do
            findTagShift [(49, "add-email"), (50, "new"), (51, "split-name")] recorded
                `shouldBe` Nothing
        it "ignores unrecorded versions when aligning, but needs at least one match" $ do
            findTagShift [(47, "unrecorded"), (48, "add-email")] recorded `shouldBe` Just 1
            findTagShift [(10, "x"), (11, "y")] recorded `shouldBe` Nothing
