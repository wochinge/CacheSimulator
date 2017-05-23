{-# LANGUAGE OverloadedStrings #-}

module Lru.LruHashSpec
    (spec)
    where

import           Test.Hspec
import           Test.QuickCheck

import           Cache
import           Lru.LruHash
import           Request

initialCache = empty 1000 AddToCache :: Lru

testRequests1 = [ (Read, "1", 500) -- fail
               , (Read, "2", 500)  -- fail
               , (Read, "3", 500)  -- fail
               , (Read, "3", 500)  -- hit
               ]

hitRequests = [ (Read, "1", 500)   -- fail
              , (Read, "1", 500)   -- hit
              , (Read, "1", 500)   -- hit
              , (Read, "1", 500)   -- hit
              ]

requestsWithRemove = [ (Read, "1", 500)   -- fail
                     , (Read, "2", 500)   -- fail
                     , (Remove, "1", 500) -- file 1 removed
                     , (Read, "1", 500)   -- fail
                     , (Read, "2", 500)   -- hit
                     , (Read, "2", 500)   -- hit
                     ]

noReadRequests = [ (Write, "1", 500) -- Nothing
                , (Write, "2", 500)  -- Nothing
                , (Remove, "2", 500) -- Nothing
                ]

requestsWithReuse = [ (Read, "1", 500) -- fail
                    , (Read, "2", 500) -- fail
                    , (Read, "1", 500) -- hit
                    , (Read, "3", 500) -- fail, 2 is removed
                    , (Read, "1", 500) -- hit
                    ]

spec :: Spec
spec = describe "Testing LRU caching" $ do
    it "Test removing of lru" $
        calculateHits testRequests1 initialCache `shouldBe` (1, 3)
    it "File should be in cache" $
        calculateHits hitRequests initialCache `shouldBe` (3,1)
    it "File 1 should be removed when remove request" $
        calculateHits requestsWithRemove initialCache `shouldBe` (2, 3)
    it "No read, so no hit or fail" $
        calculateHits noReadRequests initialCache `shouldBe` (0, 0)
    it "Reuse of file" $
        calculateHits requestsWithReuse initialCache `shouldBe` (2, 3)
