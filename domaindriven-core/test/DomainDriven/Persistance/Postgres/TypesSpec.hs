module DomainDriven.Persistance.Postgres.TypesSpec where

import Control.Exception (ErrorCall, evaluate)
import DomainDriven.Persistance.Postgres.Internal (getEventTableName)
import DomainDriven.Persistance.Postgres.Types (EventTable (..), quoteIdent)
import Test.Hspec
import Prelude

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
        it "computes name for InitialVersion" $ do
            getEventTableName (InitialVersion "valid_name") `shouldBe` "valid_name_v1"
        it "computes name for MigrateUsing" $ do
            getEventTableName
                (MigrateUsing (\_ _ _ -> pure ()) (InitialVersion "tbl"))
                `shouldBe` "tbl_v2"
        it "rejects names with unsafe characters" $ do
            evaluate (getEventTableName (InitialVersion "bad;name"))
                `shouldThrow` \(_ :: ErrorCall) -> True
        it "rejects names with double quotes" $ do
            evaluate (getEventTableName (InitialVersion "bad\"name"))
                `shouldThrow` \(_ :: ErrorCall) -> True
