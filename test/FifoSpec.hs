{-# LANGUAGE OverloadedStrings #-}

module FifoSpec
    (spec)
    where

import           Test.Hspec
import           Test.QuickCheck

import           Cache
import           Fifo
import           Request

initialCache = empty 1000 AddToCache :: Fifo

requestsExceedingMaxSize =
    [ (Read, "1", 100)
    , (Read, "2", 900)
    , (Read, "1", 100)
    , (Read, "3", 100)
    , (Read, "2", 900)
    , (Read, "1", 100)
    ]

spec :: Spec
spec = describe "Testing Fifo caching" $ do
    it "Test read with empty cache" $
        calculateHits [] initialCache `shouldBe` (0, 0)
    it "Tests whether first file in, is first file out" $
        calculateHits requestsExceedingMaxSize initialCache `shouldBe` (2, 4)
