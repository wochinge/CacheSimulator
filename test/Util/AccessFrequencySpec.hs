{-# LANGUAGE OverloadedStrings #-}

module Util.AccessFrequencySpec
( spec
) where

import           Test.Hspec
import           Test.QuickCheck

import qualified Data.Map             as M

import           Request              (FileRequest (..), RequestType (..))
import           Util.AccessFrequency (calculateAverageReadsPerFile)

emptyRequests = []
emptyStatistics = M.empty

oneRequestsPerFile = [ (Read, "1", 1)
                     , (Read, "2", 1)
                     , (Read, "5", 1300)
                     , (Read, "3", 1800)
                     ]

twoRequestsPerFile = [ (Read, "1", 1)
                     , (Read, "1", 1)
                     , (Read, "2", 1300)
                     , (Read, "2", 1800)
                     ]

spec :: Spec
spec = describe "Testing creation of file size statistics" $ do
    it "1 requests per file" $
        calculateAverageReadsPerFile oneRequestsPerFile `shouldBe` (1, 1, 1)
    it "2 requests per file" $
        calculateAverageReadsPerFile twoRequestsPerFile `shouldBe` (2, 2, 2)
