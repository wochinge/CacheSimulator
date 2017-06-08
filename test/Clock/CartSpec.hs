{-# LANGUAGE OverloadedStrings #-}

module Clock.CartSpec
    (spec)
    where

import           Test.Hspec
import           Test.QuickCheck

import           Cache
import           Clock.Cart
import           Request

initialCache = ((0, 0), empty 1000 AddToCache :: Cart)

onlyReadsWhichShouldHit = [ (Read, "1", 500) -- fail
                          , (Read, "1", 500) -- hit, pageReference = 1
                          , (Read, "1", 500) -- hit
                          , (Read, "1", 500) -- hit,
                          ]

noReadRequests = [ (Write, "1", 500) -- Nothing
                 , (Write, "2", 500)  -- Nothing
                 , (Remove, "2", 500) -- Nothing
                 ]

requestsWithRemove = [ (Write, "1", 500)
                     , (Read, "1", 500)
                     , (Read, "2", 500)
                     , (Remove, "1", 500)
                     , (Read, "1", 500)
                     ]
simpleT1Evicting = [ (Read, "1", 500)
                   , (Read, "2", 600)
                   , (Read, "1", 500)
                   ]

t1EvictingWithPageReferenceBit = [ (Read, "1", 500)
                                 , (Read, "1", 500)
                                 , (Read, "2", 500)
                                 , (Read, "3", 500)
                                 , (Read, "1", 500)
                                 ]

t1LongTerm = [ (Read, "1", 500)
             , (Read, "2", 600)
             , (Read, "1", 500) -- 1 is longterm
             , (Read, "3", 500)
             , (Read, "4", 100) -- 3 is evicted, as 1 is longterm
             , (Read, "1", 500)
             ]

spec :: Spec
spec = describe "Testing cart caching" $ do
    it "Simple test for files which should be in cache (expect for the first request)" $
        calculateHits onlyReadsWhichShouldHit initialCache `shouldBe` (3, 1)
    it "No read request = no hit or fail" $
        calculateHits noReadRequests initialCache `shouldBe` (0, 0)
    it "Test correct function of remove" $
        calculateHits requestsWithRemove initialCache `shouldBe` (1, 2)
    it "Test evicting of t1 element if cache is full" $
        calculateHits simpleT1Evicting initialCache `shouldBe` (0, 3)
    it "Test evicting from t1 if there is a file which is referenced" $
        calculateHits t1EvictingWithPageReferenceBit initialCache `shouldBe` (2, 3)
    it "Test whether a t1 file is correctly categorized as long term" $
        calculateHits t1LongTerm initialCache `shouldBe` (1, 5)
