module SimpleCaches
( readFromCache'
) where

import           Cache

readFromCache' :: Cache a => (File -> a -> (Bool, a)) -> File -> a -> (Bool, a)
readFromCache' updateCache file cache
    | file `biggerAsMax` cache = (False, cache)
    | wasInCache = (True, cache')
    | otherwise = (False, file `to` cache)
    where (wasInCache, cache') = updateCache file cache
