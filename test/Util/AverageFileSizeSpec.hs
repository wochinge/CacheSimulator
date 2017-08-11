{-# LANGUAGE OverloadedStrings #-}

module Util.AverageFileSizeSpec
( spec
) where

import           Test.Hspec
import           Test.QuickCheck

import qualified Data.Map             as M

import           Request              (FileRequest (..), RequestType (..))
import           Util.AverageFileSize (calculateAverageFileSize)

emptyRequests = []
emptyStatistics = M.empty

twoBytesPerFile =    [ (Read, "1", 1)
                     , (Read, "2", 3)
                     , (Read, "3", 1)
                     , (Read, "4", 3)
                     ]

onlySameSizes = [ (Read, "1", 100)
                , (Read, "1", 100)
                , (Read, "2", 100)
                , (Read, "3", 100)
                ]

spec :: Spec
spec = describe "Testing creation of file size statistics" $ do
    it "3 different files with average size of 1 byte" $
        calculateAverageFileSize twoBytesPerFile `shouldBe` (2, 4)
    it "3 different files with average size of 100 bytes" $
        calculateAverageFileSize onlySameSizes `shouldBe` (100, 3)
