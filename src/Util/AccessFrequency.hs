{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Util.AccessFrequency
( getAverageReadsPerFile
, calculateAverageReadsPerFile
) where

import           Data.List       (foldl', sort)
import qualified Data.Map.Strict as M
import           Request         (FileID, FileRequest, RequestType (..),
                                  forEachFileRequestIn)

type FileStatistic = M.Map FileID Int

getAverageReadsPerFile :: String -> IO (Double, Int, Int, Int)
getAverageReadsPerFile logPath =
    calculateAverageReadsPerFile `forEachFileRequestIn` logPath

calculateAverageReadsPerFile :: [FileRequest] -> (Double, Int, Int, Int)
calculateAverageReadsPerFile requests =
    let statistic = foldl' fileToStatistic M.empty requests
        sumOfAllReads = fromIntegral $ foldl' (+) 0 statistic
        sumOfDifferentFiles = fromIntegral $ M.size statistic
        maxReads = foldl' max 0 statistic
        minReads = foldl' min maxReads statistic
        median = medianAccessRate statistic
    in  (sumOfAllReads / sumOfDifferentFiles, minReads, maxReads, median)

fileToStatistic :: FileStatistic -> FileRequest -> FileStatistic
fileToStatistic statistic (Read, fileId, _) =
    let combine oldValue newValue = oldValue + newValue
    in M.insertWith combine fileId 1 statistic
fileToStatistic statistic _ = statistic

medianAccessRate :: FileStatistic -> Int
medianAccessRate statistic =
    let ordered = sort . M.elems $ statistic
        indexOfMedian = length ordered `div` 2
    in ordered !! indexOfMedian
