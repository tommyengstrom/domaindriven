module Data.ShapeCoerceSpec (spec) where

import Data.ShapeCoerce (shapeCoerce)
import Data.ShapeCoerce.V1 qualified as V1
import Data.ShapeCoerce.V2 qualified as V2
import Test.Hspec
import Prelude

spec :: Spec
spec =
    describe "ShapeCoercible widening" $ do
        describe "one added constructor" $ do
            it "supports insertion before all source constructors" $ do
                map
                    shapeCoerce
                    [ V1.StartFirst
                    , V1.StartSecond 12
                    , V1.StartThird True
                    ]
                    `shouldBe` [ V2.StartFirst
                               , V2.StartSecond 12
                               , V2.StartThird True
                               ]

            it "supports insertion after the first source constructor" $ do
                map
                    shapeCoerce
                    [ V1.AfterFirstFirst
                    , V1.AfterFirstSecond 23
                    , V1.AfterFirstThird False
                    ]
                    `shouldBe` [ V2.AfterFirstFirst
                               , V2.AfterFirstSecond 23
                               , V2.AfterFirstThird False
                               ]

            it "supports insertion after the second source constructor" $ do
                map
                    shapeCoerce
                    [ V1.AfterSecondFirst
                    , V1.AfterSecondSecond 34
                    , V1.AfterSecondThird True
                    ]
                    `shouldBe` [ V2.AfterSecondFirst
                               , V2.AfterSecondSecond 34
                               , V2.AfterSecondThird True
                               ]

            it "supports insertion after all source constructors" $ do
                map
                    shapeCoerce
                    [ V1.EndFirst
                    , V1.EndSecond 45
                    , V1.EndThird False
                    ]
                    `shouldBe` [ V2.EndFirst
                               , V2.EndSecond 45
                               , V2.EndThird False
                               ]

        it "supports consecutive added constructors" $ do
            map
                shapeCoerce
                [ V1.ConsecutiveFirst
                , V1.ConsecutiveSecond "payload"
                , V1.ConsecutiveThird True
                ]
                `shouldBe` [ V2.ConsecutiveFirst
                           , V2.ConsecutiveSecond "payload"
                           , V2.ConsecutiveThird True
                           ]

        it "supports constructors added in multiple places" $ do
            map
                shapeCoerce
                [ V1.MultipleFirst
                , V1.MultipleSecond 56
                , V1.MultipleThird False
                ]
                `shouldBe` [ V2.MultipleFirst
                           , V2.MultipleSecond 56
                           , V2.MultipleThird False
                           ]

        describe "a one-constructor source" $ do
            it "supports an added constructor before the source constructor" $ do
                shapeCoerce (V1.BeforeSingletonOriginal 67)
                    `shouldBe` V2.BeforeSingletonOriginal 67

            it "supports an added constructor after the source constructor" $ do
                shapeCoerce (V1.AfterSingletonOriginal "singleton")
                    `shouldBe` V2.AfterSingletonOriginal "singleton"

        it "preserves equal-sized sum coercion" $ do
            map
                shapeCoerce
                [ V1.EqualFirst
                , V1.EqualSecond 78
                , V1.EqualThird True
                ]
                `shouldBe` [ V2.EqualFirst
                           , V2.EqualSecond 78
                           , V2.EqualThird True
                           ]
