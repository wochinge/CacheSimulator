module SimpleCaches
    ( readFromCache'
    )
    where
import Cache

readFromCache' :: Cache a => (File -> a -> (Bool, a)) -> File -> a -> (Bool, a)
readFromCache' updateCache file cache =
    let (wasInCache, updatedCache) = updateCache file cache
        newCache = if not wasInCache then file `to` cache else updatedCache
    in (wasInCache, newCache)
