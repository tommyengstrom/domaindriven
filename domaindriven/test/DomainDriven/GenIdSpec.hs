module DomainDriven.GenIdSpec (spec) where

import Data.UUID qualified as UUID
import DomainDriven
import Effectful
import Effectful.State.Static.Local
import Test.Hspec
import Prelude

spec :: Spec
spec =
    describe "GenId" $ do
        it "runs with a fixed supplier under runPureEff without IOE" $ do
            let expected = UUID.fromWords 1 2 3 4
                actual = runPureEff $ runGenIdWith (pure expected) genId

            actual `shouldBe` expected

        it "invokes a state-backed supplier once per request" $ do
            let firstId = UUID.fromWords 1 0 0 0
                secondId = UUID.fromWords 2 0 0 0
                unusedId = UUID.fromWords 3 0 0 0
                (actual, remaining) =
                    runPureEff
                        . runState [firstId, secondId, unusedId]
                        . runGenIdWith
                            ( state \case
                                nextId : remainingIds -> (nextId, remainingIds)
                                [] -> error "test GenId supplier exhausted"
                            )
                        $ (,) <$> genId <*> genId

            actual `shouldBe` (firstId, secondId)
            remaining `shouldBe` [unusedId]

        it "runs the production interpreter" $ do
            () <$ runEff (runGenId genId)
