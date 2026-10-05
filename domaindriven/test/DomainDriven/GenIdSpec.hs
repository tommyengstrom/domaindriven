module DomainDriven.GenIdSpec (spec) where

import Data.UUID qualified as UUID
import Data.Word (Word32)
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
            let firstId :: UUID.UUID
                firstId = UUID.fromWords 1 0 0 0
                secondId :: UUID.UUID
                secondId = UUID.fromWords 2 0 0 0
                actual :: (UUID.UUID, UUID.UUID)
                nextCounter :: Word32
                (actual, nextCounter) =
                    runPureEff
                        . runState (1 :: Word32)
                        . runGenIdWith
                            (state $ \next -> (UUID.fromWords next 0 0 0, next + 1))
                        $ (,) <$> genId <*> genId

            actual `shouldBe` (firstId, secondId)
            nextCounter `shouldBe` 3

        it "runs the production interpreter" $ do
            () <$ runEff (runGenId genId)
