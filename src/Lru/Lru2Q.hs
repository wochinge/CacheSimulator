module Lru.Lru2Q
    ( Lru2Q(..)
    )

where

import qualified Cache       as C
import qualified Lru.LruHash as Lru

data Lru2Q = Lru2Q { am            :: Lru.Lru
                   , ain           :: Lru.Lru
                   , aout          :: Lru.Lru
                   , maxSize       :: C.CacheSize
                   , writeStrategy :: C.WriteStrategy
                   }

instance C.Cache Lru2Q where
    empty = empty'
    readFromCache = readFromCache
    to file cache = file `toAin` cache
    remove = remove'
    size cache = C.size (am cache) + C.size (ain cache)
    maxSize = maxSize
    writeStrategy = writeStrategy

empty' :: C.CacheSize -> C.WriteStrategy-> Lru2Q
empty' maxCacheSize ws =
    let amList = C.empty maxCacheSize ws :: Lru.Lru
        ainList = C.empty maxCacheSize ws
        aoutList = C.empty (maxCacheSize `div` 2) ws
    in Lru2Q amList ainList aoutList maxCacheSize ws

remove' :: C.File -> Lru2Q -> Lru2Q
remove' file cache =
    let ain' = ain cache
        aout' = aout cache
        (wasInAmCache, updatedAm) = Lru.removeMaybe file $ am cache
        (wasInAinCache, updatedAin) = if not wasInAmCache then Lru.removeMaybe file ain' else (True, ain')
        (_, updatedAout) = if not wasInAinCache then Lru.removeMaybe file aout' else (True, aout')
    in cache {am = updatedAm, ain = updatedAin, aout = updatedAout}

readFromCache :: C.File -> Lru2Q -> (Bool, Lru2Q)
readFromCache file cache
    | inAin = (True, cache)
    | inAm = (True, cache { am = am' })
    | inAout = (False, file `toAm` cache { aout = aout' })
    | otherwise = (False, file `toAin` cache)
    where (inAm, am') = file `Lru.updateCache` am cache
          (inAout, aout') = file `Lru.removeMaybe` aout cache
          inAin = file `Lru.inCache` ain cache

toAin :: C.File -> Lru2Q -> Lru2Q
toAin file@(_, fileSize) cache
    | fileSize > maxSize cache = cache
    | file `C.fits` cache = cache { ain = file `C.to` ain cache }
    | otherwise = file `toAin` free2Q cache

toAm :: C.File -> Lru2Q -> Lru2Q
toAm file@(_, fileSize) cache
    | fileSize > maxSize cache = cache
    | file `C.fits` cache = cache { am = file `C.to` am cache }
    | otherwise = file `toAm` free2Q cache

free2Q :: Lru2Q -> Lru2Q
free2Q cache
    | C.size (ain cache) > maximumSizeAIn (maxSize cache) || amIsEmpty = cache {ain = ain', aout = removed `C.to` aout cache }
    | otherwise = cache {am = Lru.removeLRU $ am cache}
    where (removed, ain') = Lru.removeAndRetrieveLRU $ ain cache
          amIsEmpty = 0 == C.size (am cache)

maximumSizeAIn :: C.CacheSize -> C.CacheSize
maximumSizeAIn maxCacheSize = maxCacheSize `div` 4
