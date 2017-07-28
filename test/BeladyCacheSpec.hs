{-# LANGUAGE OverloadedStrings #-}

module BeladyCacheSpec
    (spec)
    where

import           Test.Hspec
import           Test.QuickCheck

import           BeladyCache
import           Cache
import           Request

initialCache = ((0, 0), empty 1000 AddToCache :: BeladyCache)

requestsFittingInCache = [ (Read, "1", 500)
                         , (Read, "2", 500)
                         , (Read, "1", 500)
                         , (Read, "2", 500)
                         ] -- only hits

requestsExceedingMaxCacheSize = [ (Read, "1", 500) -- fail
                                , (Read, "2", 500) -- fail
                                , (Read, "3", 500)  -- fail, 1 removed
                                , (Read, "3", 500)  -- hit
                                , (Read, "2", 500)  -- hit
                                , (Read, "2", 500)  -- hit
                                , (Read, "1", 500)  -- fail
                                ]

requestsWithRemove = [ (Read, "1", 500) -- fail
                     , (Read, "2", 500) -- fail
                     , (Read, "3", 500) -- fail
                     , (Remove, "1", 500)
                     , (Read, "1", 500) -- fail
                     , (Read, "2", 500) -- hit
                     ]

requestWithInitialRemove = [ (Remove, "1", 500)
                           , (Read, "1", 500)
                           , (Read, "1", 500)
                           , (Read, "2", 500)
                           ]

requestWithSeveralInitialRemoves = [ (Write, "1", 500)
                                   , (Remove, "1", 500)
                                   , (Read, "1", 500)
                                   , (Read, "2", 500)
                                   , (Read, "3", 100)
                                   , (Read, "1", 500)
                                   , (Read, "2", 500)
                                   ]

hitsWith :: [FileRequest] -> (Int, Int)
hitsWith requests =
    let (stats, initial) = initialCache
    in calculateHits requests (stats, initFuture requests 100 initial)

spec :: Spec
spec = describe "Testing ideal caching" $ do
    it "Simple test for files which should be in cache (expect for the first requests)" $
        hitsWith requestsFittingInCache `shouldBe` (2, 2)
    it "Test with files exceeding space (file with most future access should be evicted)" $
        hitsWith requestsExceedingMaxCacheSize `shouldBe` (3, 4)
    it "Test with write requests" $
        hitsWith requestsWithRemove `shouldBe` (1, 4)
    it "Test what happens if a remove is there before a read" $
        hitsWith requestWithInitialRemove `shouldBe` (1, 2)
    it "Test several removes before a read" $
        hitsWith requestWithSeveralInitialRemoves `shouldBe` (1, 4)
