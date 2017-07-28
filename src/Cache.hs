
module Cache
    ( File
    , CacheSize
    , CacheStatistic
    , getCacheStatistic
    , calculateHits
    , Cache (..)
    , fits
    , WriteStrategy (..)
    , biggerAsMax
    )
    where

import           Prelude   hiding (fail)
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
getCacheStatistic fileName cache = (`calculateHits` ((0, 0), cache)) `forEachFileRequestIn` fileName
--getCacheStatistic fileName cache = (fst . foldl' calculateHits' ((0, 0), cache)) `forEachFileRequestIn` fileName

calculateHits :: Cache a => [FileRequest] -> (CacheStatistic, a) -> CacheStatistic
calculateHits [] (stats, _) = stats
calculateHits ((Read, fileID, fileSize) : rest) (stats, cache) =
  let (readResult, updatedCache) = readFromCache (fileID, fileSize) cache
  in calculateHits rest (boolToCacheStatistic readResult stats, updatedCache)
calculateHits ((Write, fileID, fileSize) : rest) (stats, cache)
  | writeStrategy cache == Invalidate = calculateHits rest (stats, cacheWithoutFile)
  | otherwise = calculateHits rest (stats, file `to` cacheWithoutFile)
    where file = (fileID, fileSize)
          cacheWithoutFile = remove file cache -- invalidate possible old version before adding it to the cache
calculateHits ((Remove, fileID, fileSize) : rest) (stats, cache) =
  let file = (fileID, fileSize)
      updatedCache = remove file cache
  in calculateHits rest (stats, updatedCache)

calculateHits' :: Cache a => (CacheStatistic, a) -> FileRequest -> (CacheStatistic, a)
calculateHits' (stats, cache) (Read, fileID, fileSize) =
    let (readResult, cache') = readFromCache (fileID, fileSize) cache
    in (boolToCacheStatistic readResult stats, cache')
calculateHits' (stats, cache) (Write, fileID, fileSize)
  | writeStrategy cache == Invalidate = (stats, cacheWithoutFile)
  | otherwise = (stats, file `to` cacheWithoutFile)
    where file = (fileID, fileSize)
          cacheWithoutFile = remove file cache
calculateHits' (stats, cache) (Remove, fileID, fileSize) =
  let file = (fileID, fileSize)
      cache' = remove file cache
  in (stats, cache')

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

biggerAsMax :: Cache a => File -> a -> Bool
biggerAsMax (_, fileSize) cache = fileSize > maxSize cache
