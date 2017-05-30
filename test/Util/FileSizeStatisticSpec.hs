{-# LANGUAGE OverloadedStrings #-}

module Util.FileSizeStatisticSpec
( spec
) where

import           Test.Hspec
import           Test.QuickCheck

import qualified Data.Map               as M

import           Request                (FileRequest (..), RequestType (..))
import           Util.FileSizeStatistic (getFileStatistic)

emptyRequests = []
emptyStatistics = M.empty

requests = [ (Read, "1", 1100)
           , (Read, "1", 1150)
           , (Read, "2", 1300)
           , (Read, "4", 1800)
           ]
requestsStatistic = M.fromList [(1000, 2), (2000, 1)]

spec :: Spec
spec = describe "Testing creation of file size statistics" $ do
    it "Test empty statistic" $
        getFileStatistic emptyRequests `shouldBe` emptyStatistics
    it "Test creation of statistic" $
        getFileStatistic requests `shouldBe` requestsStatistic
