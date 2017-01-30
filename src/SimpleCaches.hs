module SimpleCaches
    ( readFromCache'
    )
    where
import Cache

readFromCache' :: Cache a => (File -> a -> (Bool, a)) -> File -> a -> (Bool, a)
readFromCache' updateCache file cache =
    let (wasUpdated, updatedCache) = updateCache file cache
        newCache = if not wasUpdated then file `to` cache else updatedCache
    in (wasUpdated, newCache)
