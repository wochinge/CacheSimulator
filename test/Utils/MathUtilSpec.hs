{-# LANGUAGE OverloadedStrings #-}

module Utils.MathUtilSpec
( spec
)
where

import Test.Hspec
import Test.QuickCheck

import Utils.MathUtil (toBounds)

spec :: Spec
spec = describe "Test math utils" $ do
    it "Test whether a number remains untouched if it is in the bounds" $
        5 `toBounds` (0, 10) `shouldBe` 5
    it "Test whether a number is set to the minimum bound if it is below the minimum bound" $
        0 `toBounds` (1, 10) `shouldBe` 1
    it "Test whether a number is set to the max bound if it is above the maximum bound" $
        11 `toBounds` (1, 10) `shouldBe` 10
