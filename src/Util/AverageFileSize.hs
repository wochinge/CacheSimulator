{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Util.AverageFileSize
( getAverageFileSize
, calculateAverageFileSize
) where

import           Data.List       (foldl')
import qualified Data.Map.Strict as M
import           Request         (FileID, FileRequest, FileSize,
                                  RequestType (..), forEachFileRequestIn)

type FileStatistic = M.Map FileID FileSize

getAverageFileSize :: String -> IO (Double, Int)
getAverageFileSize logPath =
    calculateAverageFileSize `forEachFileRequestIn` logPath

calculateAverageFileSize :: [FileRequest] -> (Double, Int)
calculateAverageFileSize requests =
    let statistic = foldl' fileToStatistic M.empty requests
        sumOfAllSizes = fromIntegral $ foldl' (+) 0 statistic
        nrOfDifferentFiles = M.size statistic
    in  (sumOfAllSizes / fromIntegral nrOfDifferentFiles, nrOfDifferentFiles)

fileToStatistic :: FileStatistic -> FileRequest -> FileStatistic
fileToStatistic statistic (Read, fileId, fileSize) =
    let combine oldValue _ = oldValue
    in M.insertWith combine fileId fileSize statistic
fileToStatistic statistic _ = statistic
