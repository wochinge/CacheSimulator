module Main where

import qualified BeladyCache            as Belady
import           Cache                  (Cache, CacheSize, CacheStatistic,
                                         WriteStrategy (..), empty,
                                         getCacheStatistic)
import qualified Clock.Car              as Car
import qualified Clock.Cart             as Cart
import           Control.Monad          (when)
import           Data.Semigroup         ((<>))
import           Data.Time
import qualified Fifo
import qualified Lfu
import qualified Lru.Lru2Q              as Lru2Q
import qualified Lru.LruHash            as Lru
import           Options.Applicative    hiding (empty)
import           Util.CacheSize         (maxCacheSize)
import           Util.FileSizeStatistic (saveFileStatisticTo)

data Opts = Opts
    { calcSize         :: !Bool
    , lru              :: !Bool
    , lfu              :: !Bool
    , lru2Q            :: !Bool
    , car              :: !Bool
    , cart             :: !Bool
    , fifo             :: !Bool
    , belady           :: !Bool
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
         <*> switch (long "lfu" <> help "Test with a lfu cache")
         <*> switch (long "2q" <> help "Test with a 2q cache")
         <*> switch (long "car" <> help "Test with a car cache")
         <*> switch (long "cart" <> help "Test with a cart cache")
         <*> switch (long "fifo" <> help "Test with a fifo cache")
         <*> switch (long "belady" <> help "Test with a Belady cache")
         <*> strOption
                (long "logfile" <> metavar "PATH" <> help "Path to the log file with file requests")
         <*> option auto
               (long "cacheSize" <> metavar "VALUE_IN_BYTE" <> value 1000000000 <> help "Cache size in byte")
         <*> switch (long "fileStatistic" <> help "Print statistic of file sizes to file")
         <*> switch (long "writeAddsToCache" <> help "If true a write also adds the file to the request")

main :: IO ()
main = do
    options@Opts {logfile=logPath, cacheSize=sizeOfCache, writeAddsToCache=writeAdds} <- execParser optsParser

    putStrLn $ "Simulation with log "
        ++ logPath
        ++ ", size of " ++ show (cacheSize options)
        ++ " bytes and writeAddsToCache = " ++ show (writeAddsToCache options)
        ++ "\n"

    let args = (logPath, sizeOfCache, if writeAdds then AddToCache else Invalidate)
    when (calcSize options) $ time $ calculateMaxCacheSize logPath
    when (lru options) $ time $ calculateLru args
    when (lfu options) $ time $ calculateLfu args
    when (lru2Q options) $ time $ calculateLru2Q args
    when (car options) $ time $ calculateCar args
    when (cart options) $ time $ calculateCart args
    when (fifo options) $ time $ calculateFifo args
    when (belady options) $ time $ calculateBelady args
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

simulate :: Cache a => String -> String -> a -> IO ()
simulate algorithmName logPath emptyCache = do
    putStrLn $ "Run " ++ algorithmName
    stat <- getCacheStatistic logPath emptyCache
    printStatistic algorithmName stat

printStatistic :: String -> CacheStatistic -> IO ()
printStatistic algorithmName (hits, fails) = do
    putStrLn $ algorithmName ++ " had " ++ show hits ++ " hits"
    putStrLn $ algorithmName ++ " had " ++ show fails ++ " fails"

calculateLru :: (String, CacheSize, WriteStrategy) -> IO ()
calculateLru (logPath, sizeOfCache, strategy) = do
    let cache = empty sizeOfCache strategy :: Lru.Lru
    simulate "LRU" logPath cache
calculateLfu :: (String, CacheSize, WriteStrategy) -> IO()
calculateLfu (logPath, sizeOfCache, strategy) = do
    let cache = empty sizeOfCache strategy :: Lfu.Lfu
    simulate "LFU" logPath cache

calculateLru2Q :: (String, CacheSize, WriteStrategy) -> IO()
calculateLru2Q (logPath, sizeOfCache, strategy) = do
    let cache = empty sizeOfCache strategy :: Lru2Q.Lru2Q
    simulate "2Q" logPath cache

calculateCar :: (String, CacheSize, WriteStrategy) -> IO()
calculateCar (logPath, sizeOfCache, strategy) = do
    let cache = empty sizeOfCache strategy :: Car.Car
    simulate "Car" logPath cache

calculateCart :: (String, CacheSize, WriteStrategy) -> IO()
calculateCart (logPath, sizeOfCache, strategy) = do
    let cache = empty sizeOfCache strategy :: Cart.Cart
    simulate "Cart" logPath cache

calculateFifo :: (String, CacheSize, WriteStrategy) -> IO()
calculateFifo (logPath, sizeOfCache, strategy) = do
    let cache = empty sizeOfCache strategy :: Fifo.Fifo
    simulate "Fifo" logPath cache

calculateBelady :: (String, CacheSize, WriteStrategy) -> IO()
calculateBelady (logPath, sizeOfCache, strategy) = do
    putStrLn $ "Run " ++ "Belady"
    let cache = empty sizeOfCache strategy :: Belady.BeladyCache
    stat <- Belady.getBeladyCacheStatistic cache logPath
    printStatistic "Belady" stat
