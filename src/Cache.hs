module Cache
    ( File
    , CacheSize
    , CacheStatistic
    , getCacheStatistic
    , calculateHits
    , Cache(..)
    , fits
    )
    where

import System.IO (withFile, IOMode(..))
import Prelude hiding (fail)
import Request
import Data.Maybe (isJust)

type File = (FileID, FileSize)
type CacheSize = Int
type CacheStatistic = (Int, Int)

class Cache a where
  to :: File -> a -> a
  update :: File -> a -> (Bool, a)
  remove :: File -> a -> a
  empty :: CacheSize -> a
  size :: a -> CacheSize
  maxSize :: a -> CacheSize

getCacheStatistic :: Cache a => String -> a -> IO CacheStatistic
getCacheStatistic fileName cache = (`calculateHits` cache) `forEachFileRequestIn` fileName

calculateHits :: Cache a => [FileRequest] -> a -> CacheStatistic
calculateHits [] _ = (0, 0)
calculateHits ((Read, fileID, fileSize) : rest) cache =
  let file = (fileID, fileSize)
      (itemWasInCache, updatedCache) = file `update` cache
      newCache = if not itemWasInCache then file `to` cache else updatedCache
  in boolToCacheStatistic itemWasInCache $ calculateHits rest newCache
calculateHits ((Write, fileID, fileSize) : rest) cache =
    let file = (fileID, fileSize)
        updatedCache = file `to` cache
    in calculateHits rest updatedCache
calculateHits ((Remove, fileID, fileSize) : rest) cache =
  let file = (fileID, fileSize)
      updatedCache = remove file cache
  in calculateHits rest updatedCache

boolToCacheStatistic :: Bool -> CacheStatistic -> CacheStatistic
boolToCacheStatistic True = hit
boolToCacheStatistic _ = fail

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
