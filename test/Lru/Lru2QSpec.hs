{-# LANGUAGE OverloadedStrings #-}

module Lru.Lru2QSpec
    (spec)
    where

import           Test.Hspec
import           Test.QuickCheck

import           Cache
import           Lru.Lru2Q
import           Request

initialCache = ((0, 0), empty 1000 AddToCache :: Lru2Q)

onlyReadsWhichShouldHit = [ (Read, "1", 500) -- fail
                          , (Read, "2", 500)  -- fail
                          , (Read, "1", 500)  -- hit
                          , (Read, "2", 500)  -- hit
                          , (Read, "1", 500)  -- hit
                          , (Read, "2", 500)  -- hit
                          , (Read, "1", 500)  -- hit
                          , (Read, "2", 500)  -- hit
                          ]

filesExceedingMaxSize = [ (Read, "1", 500) -- fail
                        , (Read, "2", 500) -- fail
                        , (Read, "3", 500) -- fail, file 1 goes to aout
                        , (Read, "1", 500) -- fail, fail 1 goes to am, file 2 goes to aout
                        , (Read, "1", 500) -- hit
                        , (Read, "3", 500) -- hit
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

testAmCache = [ (Read, "1", 400)  -- fail
              , (Read, "2", 400)  -- fail
              , (Read, "3", 200)  -- fail
              , (Read, "1", 400)  -- hit
              , (Read, "4", 400)  -- fail, 1 goes to aout
              , (Read, "1", 400)  -- fail, 1 goes to am
              , (Read, "3", 200)  -- hit
              , (Read, "4", 400)  -- hit
              , (Read, "2", 400)  -- fail, 3 goes to aout
              , (Read, "1", 400)  -- hit, still in am
              , (Read, "3", 200)  -- fail
              ]
spec :: Spec
spec = describe "Testing 2Q caching" $ do
    it "Simple test for files which should be in cache (expect for the first request)" $
        calculateHits onlyReadsWhichShouldHit initialCache `shouldBe` (6, 2)
    it "Test where the requested files exceed the maximal cache size" $
        calculateHits filesExceedingMaxSize initialCache `shouldBe` (2, 4)
    it "Test manual removing of file" $
        calculateHits requestsWithRemove initialCache `shouldBe` (2, 3)
    it "No read, so no hit or fail" $
        calculateHits noReadRequests initialCache `shouldBe` (0, 0)
    it "Test correct am cache" $
        calculateHits testAmCache initialCache `shouldBe` (4, 7)
