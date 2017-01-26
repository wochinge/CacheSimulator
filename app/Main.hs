module Main where

import Options.Applicative
import Data.Time
import Control.Monad (when)

import CacheSize (maxCacheSize)
import Cache (CacheSize, getCacheStatistic)
import qualified LruHash as Lru

data Opts = Opts
    { calcSize :: !Bool
    , lru :: !Bool
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
         <*> strOption
                (long "logfile" <> metavar "PATH" <> help "Path to the log file with file requests")
         <*> option auto
               (long "cacheSize" <> metavar "VALUE_IN_BYTE" <> value 1000000000 <> help "Cache size in byte")


main :: IO ()
main = do
    Opts {calcSize=calcSize, lru=calcLru, logfile=logPath, cacheSize=sizeOfCache} <- execParser optsParser
    when calcSize $ time $ calculateMaxCacheSize logPath
    when calcLru $ time $ calculateLru logPath sizeOfCache

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

calculateLru :: String -> CacheSize -> IO ()
calculateLru logPath cacheSize = do
    let cache = Lru.empty cacheSize :: Lru.Lru
    (hits, fails) <- getCacheStatistic logPath cache
    putStrLn $ "Lru had " ++ show hits ++ " hits"
    putStrLn $ "Lru had " ++ show fails ++ " fails"
