{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Util.FileSizeStatistic
( saveFileStatisticTo
, getFileStatistic
) where

import qualified Data.ByteString.Lazy.Char8 as B (pack, unlines, writeFile)
import qualified Data.Map.Strict            as M
import qualified Data.Set                   as S
import           Request                    (FileID, FileRequest, FileSize,
                                             forEachFileRequestIn)


type FileStatistic = M.Map FileSize Int
type GatheredFiles = S.Set FileID

fileStatisticPrecision :: Int
fileStatisticPrecision = 10000

saveFileStatisticTo :: String -> String -> IO ()
saveFileStatisticTo logFileName targetName = do
    statistic <- getFileStatistic `forEachFileRequestIn` logFileName
    let stringStatistic = map (\(k, v) -> B.pack $ show k ++ " " ++ show v) $ M.toList statistic
    B.writeFile targetName $ B.unlines stringStatistic

getFileStatistic :: [FileRequest] -> FileStatistic
getFileStatistic requests = snd $ foldl fileToStatistic (S.empty, M.empty) requests

fileToStatistic :: (GatheredFiles, FileStatistic) -> FileRequest -> (GatheredFiles, FileStatistic)
fileToStatistic (alreadyGathered, statistic) (_, fileId, fileSize)
    | fileId `S.member` alreadyGathered = (alreadyGathered, statistic)
    | otherwise = (S.insert fileId alreadyGathered, fileSize `to` statistic)

to :: FileSize -> FileStatistic -> FileStatistic
to fileSize statistic =
    let roundedFileSize = categorizeFileSize fileSize
        statisticForSize = M.lookup roundedFileSize statistic
    in case statisticForSize of
        Nothing -> M.insert roundedFileSize 1 statistic
        Just nrOfFilesForThisSize -> M.insert roundedFileSize (nrOfFilesForThisSize + 1) statistic

categorizeFileSize :: FileSize -> FileSize
categorizeFileSize preciseByteNumber
    | preciseByteNumber == 0 = 0
    | otherwise = max 1 $ fileStatisticPrecision * round (fromIntegral preciseByteNumber / fromIntegral fileStatisticPrecision)
