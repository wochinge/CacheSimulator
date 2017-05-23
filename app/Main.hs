module Main where

import           Cache                  (Cache, CacheSize, WriteStrategy (..),
                                         empty, getCacheStatistic)
import qualified Clock.Car              as Car
import qualified Clock.Cart             as Cart
import           Control.Monad          (when)
import           Data.Time
import           Fifo
import qualified Lru.Lru2Q              as Lru2Q
import qualified Lru.LruHash            as Lru
import qualified Mfu
import           Options.Applicative    hiding (empty)
import           Util.CacheSize         (maxCacheSize)
import           Util.FileSizeStatistic (saveFileStatisticTo)

data Opts = Opts
    { calcSize         :: !Bool
    , lru              :: !Bool
    , mfu              :: !Bool
    , lru2Q            :: !Bool
    , car              :: !Bool
    , cart             :: !Bool
    , fifo             :: !Bool
    , logfile          :: !String
    , cacheSize        :: !Int
    , fileStatistic    :: !Bool
    , writeAddsToCache :: !Bool
    }

optsParser :: ParserInfo Opts
optsParser = info
    (helper <*> programOptions)
    (fullDesc <> progDesc "Please provide a logfile, where each line consists of the following information: \n\
        \<date> <time> <read || write || remove> <unique fileID> <file size in bytes>" <>
     header
         "CacheSimulator - a program to calculate the effiency of different caching algorithms.")

programOptions :: Parser Opts
programOptions =
    Opts <$> switch (long "calcSize" <> help "Just calculate the minimal space need to store the data")
         <*> switch (long "lru" <> help "Test with a lru cache")
         <*> switch (long "mfu" <> help "Test with a mfu cache")
         <*> switch (long "2q" <> help "Test with a 2q cache")
         <*> switch (long "car" <> help "Test with a car cache")
         <*> switch (long "cart" <> help "Test with a cart cache")
         <*> switch (long "fifo" <> help "Test with a fifo cache")
         <*> strOption
                (long "logfile" <> metavar "PATH" <> help "Path to the log file with file requests")
         <*> option auto
               (long "cacheSize" <> metavar "VALUE_IN_BYTE" <> value 1000000000 <> help "Cache size in byte")
         <*> switch (long "fileStatistic" <> help "Print statistic of file sizes to file")
         <*> switch (long "writeAddsToCache" <> help "If true a write also adds the file to the request")

main :: IO ()
main = do
    options@Opts {logfile=logPath, cacheSize=sizeOfCache, writeAddsToCache=writeAdds} <- execParser optsParser
    let args = (logPath, sizeOfCache, if writeAdds then AddToCache else Invalidate)
    when (calcSize options) $ time $ calculateMaxCacheSize logPath
    when (lru options) $ time $ calculateLru args
    when (mfu options) $ time $ calculateMfu args
    when (lru2Q options) $ time $ calculateLru2Q args
    when (car options) $ time $ calculateCar args
    when (cart options) $ time $ calculateCart args
    when (fifo options) $ time $ calculateFifo args
    when (fileStatistic options) $ time $ saveFileStatisticTo logPath (logPath ++ ".fileStat")

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

calculateLru :: (String, CacheSize, WriteStrategy) -> IO ()
calculateLru (logPath, sizeOfCache, writeStrategy) = do
    let cache = empty sizeOfCache writeStrategy :: Lru.Lru
    printStatistic "LRU" logPath cache

calculateMfu :: (String, CacheSize, WriteStrategy) -> IO()
calculateMfu (logPath, sizeOfCache, writeStrategy) = do
    let cache = empty sizeOfCache writeStrategy :: Mfu.Mfu
    printStatistic "MFU" logPath cache

calculateLru2Q :: (String, CacheSize, WriteStrategy) -> IO()
calculateLru2Q (logPath, sizeOfCache, writeStrategy) = do
    let cache = empty sizeOfCache writeStrategy :: Lru2Q.Lru2Q
    printStatistic "2Q" logPath cache

calculateCar :: (String, CacheSize, WriteStrategy) -> IO()
calculateCar (logPath, sizeOfCache, writeStrategy) = do
    let cache = empty sizeOfCache writeStrategy :: Car.Car
    printStatistic "Car" logPath cache

calculateCart :: (String, CacheSize, WriteStrategy) -> IO()
calculateCart (logPath, sizeOfCache, writeStrategy) = do
    let cache = empty sizeOfCache writeStrategy :: Cart.Cart
    printStatistic "Cart" logPath cache

calculateFifo :: (String, CacheSize, WriteStrategy) -> IO()
calculateFifo (logPath, sizeOfCache, writeStrategy) = do
    let cache = empty sizeOfCache writeStrategy :: Fifo
    printStatistic "Fifo" logPath cache
