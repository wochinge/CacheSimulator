module Main where

import Data.Time
import Control.Monad (when)
import Options.Applicative hiding (empty)
import CacheSize (maxCacheSize)
import Cache (Cache, empty, CacheSize, getCacheStatistic)
import qualified LruHash as Lru
import qualified Mfu
import qualified Lru2Q
import qualified Car

data Opts = Opts
    { calcSize :: !Bool
    , lru :: !Bool
    , mfu :: !Bool
    , lru2Q :: !Bool
    , car :: !Bool
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
         <*> switch (long "lru" <> help "Test with a lru cache")
         <*> switch (long "mfu" <> help "Test with a mfu cache")
         <*> switch (long "2q" <> help "Test with a 2q cache")
         <*> switch (long "car" <> help "Test with a car cache")
         <*> strOption
                (long "logfile" <> metavar "PATH" <> help "Path to the log file with file requests")
         <*> option auto
               (long "cacheSize" <> metavar "VALUE_IN_BYTE" <> value 1000000000 <> help "Cache size in byte")

main :: IO ()
main = do
    options@Opts {logfile=logPath, cacheSize=sizeOfCache} <- execParser optsParser
    when (calcSize options) $ time $ calculateMaxCacheSize logPath
    when (lru options) $ time $ calculateLru logPath sizeOfCache
    when (mfu options) $ time $ calculateMfu logPath sizeOfCache
    when (lru2Q options) $ time $ calculateLru2Q logPath sizeOfCache
    when (car options) $ time $ calculateCar logPath sizeOfCache

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
    let cache = empty cacheSize :: Lru.Lru
    printStatistic "LFU" logPath cache

calculateMfu :: String -> CacheSize -> IO()
calculateMfu logPath cacheSize = do
    let cache = empty cacheSize :: Mfu.Mfu
    printStatistic "MFU" logPath cache

calculateLru2Q :: String -> CacheSize -> IO()
calculateLru2Q logPath cacheSize = do
    let cache = empty cacheSize :: Lru2Q.Lru2Q
    printStatistic "2Q" logPath cache

calculateCar :: String -> CacheSize -> IO()
calculateCar logPath cacheSize = do
    let cache = empty cacheSize :: Car.Car
    printStatistic "Car" logPath cache
