{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Util.AccessFrequency
( getAverageReadsPerFile
, calculateAverageReadsPerFile
) where

import           Data.List       (foldl')
import qualified Data.Map.Strict as M
import           Request         (FileID, FileRequest,
                                  RequestType (..), forEachFileRequestIn)

type FileStatistic = M.Map FileID Int

getAverageReadsPerFile :: String -> IO Double
getAverageReadsPerFile logPath =
    calculateAverageReadsPerFile `forEachFileRequestIn` logPath

calculateAverageReadsPerFile :: [FileRequest] -> Double
calculateAverageReadsPerFile requests =
    let statistic = foldl' fileToStatistic M.empty requests
        sumOfAllReads = fromIntegral $ foldl' (+) 0 statistic
        sumOfDifferentFiles = fromIntegral $ M.size statistic
    in  sumOfAllReads / sumOfDifferentFiles

fileToStatistic :: FileStatistic -> FileRequest -> FileStatistic
fileToStatistic statistic (Read, fileId, _) =
    let combine oldValue newValue = oldValue + newValue
    in M.insertWith combine fileId 1 statistic
fileToStatistic statistic _ = statistic