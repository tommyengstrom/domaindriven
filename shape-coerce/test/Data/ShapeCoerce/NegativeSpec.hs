{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

module Data.ShapeCoerce.NegativeSpec (spec) where

import Control.Exception (TypeError (..), evaluate)
import Data.List (isInfixOf)
import Data.ShapeCoerce (shapeCoerce)
import Data.ShapeCoerce.V1 qualified as V1
import Data.ShapeCoerce.V2 qualified as V2
import Test.Hspec
import Prelude

spec :: Spec
spec = describe "rejected shape coercions" $ do
    it "explains incompatible datatype names" $ do
        evaluate (shapeCoerce V1.StartFirst :: V2.InsertAtEnd)
            `shouldThrow` (\(TypeError message) -> "Reason: Incompatible data types" `isInfixOf` message)
    it "explains narrowing a sum to a single constructor" $ do
        evaluate (shapeCoerce V2.BeforeSingletonAdded :: V1.InsertBeforeSingleton)
            `shouldThrow` (\(TypeError message) -> "Reason: Left side is a sum type but right side has a single constructor" `isInfixOf` message)
    it "explains reordered constructors during widening" $ do
        evaluate (shapeCoerce V1.ReorderedFirst :: V2.Reordered)
            `shouldThrow` (\(TypeError message) -> "Reason: Source constructors are not an ordered subset of target constructors" `isInfixOf` message)
