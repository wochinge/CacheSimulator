{-# LANGUAGE OverloadedStrings #-}

module Util.FileSizeStatisticSpec
( spec
) where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Map as M

import Util.FileSizeStatistic (getFileStatistic)
import Request (FileRequest(..), RequestType(..))

emptyRequests = []
emptyStatistics = M.empty

requests = [ (Read, "1", 110)
           , (Read, "1", 115)
           , (Read, "2", 130)
           , (Read, "4", 180)
           ]
requestsStatistic = M.fromList [(0, 0), (100, 2), (200, 1)]

spec :: Spec
spec = describe "Testing creation of file size statistics" $ do
    it "Test empty statistic" $
        getFileStatistic emptyRequests `shouldBe` emptyStatistics
    it "Test creation of statistic" $
        getFileStatistic requests `shouldBe` requestsStatistic
