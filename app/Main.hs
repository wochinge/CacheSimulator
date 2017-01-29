module Main where

import Options.Applicative
import Data.Time
import Control.Monad (when)

import CacheSize (maxCacheSize)
import Cache (Cache, CacheSize, getCacheStatistic)
import qualified LruHash as Lru
import qualified Mfu

data Opts = Opts
    { calcSize :: !Bool
    , lru :: !Bool
    , mfu :: !Bool
    , logfile :: !String
    , cacheSize :: !Int
    }

optsParser :: ParserInfo Opts
optsParser = info
    (helper <*> programOptions)
    (fullDesc <> progDesc "Please provide a logfile, where each line consists of the following information: \n\
        \<date> <time> <read || write || remove> <unique fileID> <file size in bytes>" <>
     header
         "CacheSimulator - a program to calculate the effiency of different caching algorithms.")
programOptions =
    Opts <$> switch (long "size" <> help "Just calculate the minimal space need to store the data")
         <*> switch (long "lru" <> help "Test with an lru cache")
         <*> switch (long "mfu" <> help "Test with an mfu cache")
         <*> strOption
                (long "logfile" <> metavar "PATH" <> help "Path to the log file with file requests")
         <*> option auto
               (long "cacheSize" <> metavar "VALUE_IN_BYTE" <> value 1000000000 <> help "Cache size in byte")


main :: IO ()
main = do
    Opts {calcSize=calcSize, lru=calcLru, mfu=calcMfu, logfile=logPath, cacheSize=sizeOfCache} <- execParser optsParser
    when calcSize $ time $ calculateMaxCacheSize logPath
    when calcLru $ time $ calculateLru logPath sizeOfCache
    when calcMfu $ time $ calculateMfu logPath sizeOfCache

time :: IO () -> IO ()
time ioFunc = do
    start <- getCurrentTime :: IO UTCTime
    ioFunc
    end <- getCurrentTime :: IO UTCTime
    putStrLn $ "Calculating this took: " ++ show (diffUTCTime end start)

calculateMaxCacheSize :: String -> IO ()
calculateMaxCacheSize path = do
    cacheSizeInBytes <- maxCacheSize path
    putStrLn $ "The minimal needed size to hold the data is: " ++ show cacheSizeInBytes

printStatistic :: Cache a => String -> String -> a -> IO ()
printStatistic algorithmName logPath emptyCache = do
    (hits, fails) <- getCacheStatistic logPath emptyCache
    putStrLn $ algorithmName ++ " had " ++ show hits ++ " hits"
    putStrLn $ algorithmName ++ " had " ++ show fails ++ " fails"

calculateLru :: String -> CacheSize -> IO ()
calculateLru logPath cacheSize = do
    let cache = Lru.empty cacheSize :: Lru.Lru
    printStatistic "LFU" logPath cache

calculateMfu :: String -> CacheSize -> IO()
calculateMfu logPath cacheSize = do
    let cache = Mfu.empty cacheSize :: Mfu.Mfu
    printStatistic "MFU" logPath cache
