{-# LANGUAGE OverloadedStrings #-}

module IdealCacheSpec
    (spec)
    where

import           Test.Hspec
import           Test.QuickCheck

import           Cache
import           IdealCache
import           Request

initialCache = empty 1000 AddToCache :: IdealCache

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

hitsWith :: [FileRequest] -> (Int, Int)
hitsWith requests =
    let preparedCache = initFuture requests 100 initialCache
    in calculateHits requests preparedCache

spec :: Spec
spec = describe "Testing ideal caching" $ do
    it "Simple test for files which should be in cache (expect for the first requests)" $
        hitsWith requestsFittingInCache `shouldBe` (2, 2)
    it "Test with files exceeding space (file with most future access should be evicted)" $
        hitsWith requestsExceedingMaxCacheSize `shouldBe` (3, 4)
