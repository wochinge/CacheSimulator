{-# LANGUAGE OverloadedStrings #-}

module ClockSpec
( spec
) where

import Test.Hspec
import Test.QuickCheck

import Clock
import Request

testfile = ("1", 100)
testfile' = ("2", 200)

initialClock = empty :: Clock
clockWithTestFile = testfile `to` initialClock
clockWithPageReference = snd $ testfile `inCache` (testfile' `to` (testfile `to` initialClock))

spec :: Spec
spec = describe "Testing clock structure" $ do
    it "File should be not in empty clock" $
        fst (testfile `inCache` initialClock) `shouldBe` False
    it "File should be in clock" $ do
        fst (testfile `inCache` clockWithTestFile) `shouldBe` True
        size clockWithTestFile `shouldBe` snd testfile
    it "Removing of file" $ do
        size (snd $  remove testfile clockWithTestFile) `shouldBe` 0
        fst (testfile `inCache` snd (remove testfile clockWithTestFile)) `shouldBe` False
    it "Test tick without page reference" $
        tick clockWithTestFile `shouldBe` (testfile, initialClock {nextPrio = 1})
    it "Test tick with page reference" $
        size (snd $ tick clockWithPageReference) `shouldBe` snd testfile
