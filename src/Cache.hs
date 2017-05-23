module Cache
    ( File
    , CacheSize
    , CacheStatistic
    , getCacheStatistic
    , calculateHits
    , Cache (..)
    , fits
    , WriteStrategy (..)
    )
    where

import           Prelude hiding (fail)
import           Request

type File = (FileID, FileSize)
type CacheSize = Int
type CacheStatistic = (Int, Int)

data WriteStrategy = Invalidate | AddToCache deriving (Eq, Show)

class Cache a where
  to :: File -> a -> a
  readFromCache :: File -> a -> (Bool, a)
  remove :: File -> a -> a
  empty :: CacheSize -> WriteStrategy -> a
  size :: a -> CacheSize
  maxSize :: a -> CacheSize
  writeStrategy :: a -> WriteStrategy

getCacheStatistic :: Cache a => String -> a -> IO CacheStatistic
getCacheStatistic fileName cache = (`calculateHits` cache) `forEachFileRequestIn` fileName

calculateHits :: Cache a => [FileRequest] -> a -> CacheStatistic
calculateHits [] _ = (0, 0)
calculateHits ((Read, fileID, fileSize) : rest) cache =
  let (readResult, updatedCache) = readFromCache (fileID, fileSize) cache
  in boolToCacheStatistic readResult $ calculateHits rest updatedCache
calculateHits ((Write, fileID, fileSize) : rest) cache
  | writeStrategy cache == Invalidate = calculateHits rest cacheWithoutFile
  | otherwise = calculateHits rest $ file `to` cacheWithoutFile
    where file = (fileID, fileSize)
          cacheWithoutFile = remove file cache -- invalidate possible old version before adding it to the cache
calculateHits ((Remove, fileID, fileSize) : rest) cache =
  let file = (fileID, fileSize)
      updatedCache = remove file cache
  in calculateHits rest updatedCache

boolToCacheStatistic :: Bool -> CacheStatistic -> CacheStatistic
boolToCacheStatistic True = hit
boolToCacheStatistic _    = fail

hit :: CacheStatistic -> CacheStatistic
hit (hits, fails) = (hits + 1, fails)

fail :: CacheStatistic -> CacheStatistic
fail (hits, fails) = (hits, fails + 1)

fits :: Cache a => File -> a -> Bool
fits (_, fileSize) cache =
    let cacheSize = size cache
        maxCacheSize = maxSize cache
        futureSize = fileSize + cacheSize
    in futureSize <= maxCacheSize
