{-# LANGUAGE OverloadedStrings #-}

module Clock.CartSpec
    (spec)
    where

import           Test.Hspec
import           Test.QuickCheck

import           Cache
import           Clock.Cart
import           Request

initialCache = empty 1000 AddToCache :: Cart

onlyReadsWhichShouldHit = [ (Read, "1", 500) -- fail
                          , (Read, "1", 500) -- hit, pageReference = 1
                          , (Read, "1", 500) -- hit
                          , (Read, "1", 500) -- hit,
                          ]

filesExceedingMaxSize = [ (Read, "1", 1000) -- fail
                        , (Read, "2", 1)    -- fail, 1 to b1
                        , (Read, "1", 1000) -- fail, 1 to t2
                        ]

pageReference = [ (Read, "1", 500) -- fail
                , (Read, "1", 500) -- hit, pageReference = 1
                , (Read, "2", 500) -- fail
                , (Read, "3", 500) -- fail, file 2 goes to b1, file 1 pageReference = 0
                , (Read, "1", 500) -- hit, pageReference = 1
                , (Read, "1", 500) -- hit
                ]

fileToT2 = [ (Read, "1", 500) -- fail
           , (Read, "2", 500) -- fail
           , (Read, "3", 500) -- fail, 1 to b1
           , (Read, "1", 500) -- fail, 1 to t2
           , (Read, "1", 500) -- hit
           ]

twoFilesFromT1ToB1 = [ (Read, "1", 500) -- fail
                     , (Read, "2", 500) -- fail
                     , (Read, "3", 600) -- fail, 1 and 2 to b1
                     , (Read, "3", 600) -- hit
                     ]

removeFromT2 = [ (Read, "1", 1)   -- fail, t1: (1, S, NotReferenced)
               , (Read, "2", 999) -- fail, t1: [(1, S, NotReferenced), (2, S, NotReferenced)]
               , (Read, "3", 1) -- fail, t1: [(2, S, NotReferenced), (3, S, NotReferenced)] b1: [(1, S, NotReferenced)]
               , (Read, "1", 1) -- fail, t1: [(3, S, NotReferenced), (1, L, NotReferenced)] b1: [(2, S, NotReferenced)]
               , (Read, "4", 999) -- fail, t1: [(1, L, NotReferenced), (4, S, NotReferenced)] b1: [(2, S, NotReferenced), (3, S, NotReferenced)]
               , (Read, "1", 1) -- hit
               ]

fromB2ToT2 = removeFromT2 ++
            [ (Read, "1", 800) -- hit
            ]

noReadRequests = [ (Write, "1", 500) -- Nothing
                 , (Write, "2", 500)  -- Nothing
                 , (Remove, "2", 500) -- Nothing
                 ]

spec :: Spec
spec = describe "Testing cart caching" $ do
    it "Simple test for files which should be in cache (expect for the first request)" $
        calculateHits onlyReadsWhichShouldHit initialCache `shouldBe` (3, 1)
    it "File requests should push each other out of the cache because they are exceeding the max size" $
        calculateHits filesExceedingMaxSize initialCache `shouldBe` (0, 3)
    it "File 1 was requested once more, so file 2 should be pushed out for 3" $
        calculateHits pageReference initialCache `shouldBe` (3, 3)
    it "Test if file correctly moved to t2" $
        calculateHits fileToT2 initialCache `shouldBe` (1, 4)
    it "No read request = no hit or fail" $
        calculateHits noReadRequests initialCache `shouldBe` (0, 0)
    it "In order to take a new file in the cache two old ones have to be removed" $
        calculateHits twoFilesFromT1ToB1 initialCache `shouldBe` (1, 3)
    it "T2 is over its size" $
        calculateHits removeFromT2 initialCache `shouldBe` (0, 6)
    it "File should be moved from b2 to cache" $
        calculateHits fromB2ToT2 initialCache `shouldBe` (1,6)
