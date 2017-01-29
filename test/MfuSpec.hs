{-# LANGUAGE OverloadedStrings #-}

module MfuSpec
    (spec)
    where

import Test.Hspec
import Test.QuickCheck

import Mfu
import Request
import Cache

initialCache = empty 1000 :: Mfu

testRequests1 = [ (Read, "1", 500) -- fail
                , (Read, "2", 500)  -- fail
                , (Read, "1", 500)  -- hit
                , (Read, "1", 500)  -- hit
                , (Read, "2", 500)  -- hit
                , (Read, "3", 500)  -- fail
                , (Read, "1", 500)  -- hit
                , (Read, "1", 500)  -- hit
                , (Read, "2", 500)  -- fail
                ]

hitRequests = [ (Read, "1", 500) -- fail
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

allFittingsRequests = [ (Read, "1", 200) -- fail
                      , (Read, "2", 400) -- fail
                      , (Read, "3", 400) -- fail
                      , (Read, "1", 200) -- hit
                      , (Read, "2", 400) -- hit
                      , (Read, "3", 400) -- hit
                      ]
                      
spec :: Spec
spec = describe "Testing MFU caching" $ do
    it "Test removing of mfu" $
        calculateHits testRequests1 initialCache `shouldBe` (5, 4)
    it "File should be in cache" $
        calculateHits hitRequests initialCache `shouldBe` (3,1)
    it "File 1 should be removed when remove request" $
        calculateHits requestsWithRemove initialCache `shouldBe` (2, 3)
    it "No read, so no hit or fail" $
        calculateHits noReadRequests initialCache `shouldBe` (0, 0)
    it "All files shoud fit in cache" $
        calculateHits allFittingsRequests initialCache `shouldBe` (3, 3)
