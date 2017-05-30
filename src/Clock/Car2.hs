module Clock.Car2

where

import           Cache            (CacheSize, File, WriteStrategy)
import qualified Cache            as C (Cache (..), fits, remove, size, to)
import qualified Clock.Clock      as Clock
import           Clock.ClockCache
import qualified Lru.LruHash      as Lru

data Car = Car { t1            :: Clock.Clock
               , b1            :: Lru.Lru
               , t2            :: Clock.Clock
               , b2            :: Lru.Lru
               , p             :: Double
               , maxSize       :: CacheSize
               , writeStrategy :: WriteStrategy
               }

instance C.Cache Car where
    size cache = Clock.size (t1 cache) + Clock.size (t2 cache)
    maxSize = maxSize
    remove = remove
    readFromCache = readFromCache
    to = to
    empty maxCacheSize ws = Car { t1 = Clock.empty
                                           , t2 = Clock.empty
                                           , b1 = C.empty maxCacheSize ws
                                           , b2 = C.empty maxCacheSize ws
                                           , p = 0
                                           , maxSize = maxCacheSize
                                           , writeStrategy = ws
                                           }
    writeStrategy = writeStrategy

instance ClockCache Car where
 getP = p
 setP p' cache = cache {p = p'}
 sizeOfT1 = Clock.size . t1
 sizeOfT2 = Clock.size . t2

readFromCache :: File -> Car -> (Bool, Car)
readFromCache file cache =
    let (inT1, updatedT1) = file `Clock.inCache` t1 cache
        (inT2, updatedT2) = if not inT1 then file `Clock.inCache` t2 cache else (False, t2 cache)
    in if inT1 || inT2
        then (True, cache {t1 = updatedT1, t2 = updatedT2})
        else (False, file `to` cache)

to :: File -> Car -> Car
to file cache
    | not inGhostCache = preparedCache {t1 = file `Clock.to` t1 preparedCache}
    | inB1 = fromB1toT2 file . incP $ preparedCache
    | otherwise = fromB2toT2 file . decP $ preparedCache
    where
        inB1 = file `Lru.inCache` b1 cache
        inB2 = not inB1 && file `Lru.inCache` b2 cache
        inGhostCache = inB1 || inB2
        preparedCache = replaceGhostCaches inGhostCache file $ replace file cache

fromB1toT2 :: File -> Car -> Car
fromB1toT2 file cache =
    let t2Clock = t2 cache
    in cache {t2 = file `Clock.to` t2Clock, b1 = file `C.remove` b1 cache}

fromB2toT2 :: File -> Car -> Car
fromB2toT2 file cache =
    let t2Clock = t2 cache
    in cache {t2 = file `Clock.to` t2Clock, b2 = file `C.remove` b2 cache}

replace :: File -> Car -> Car
replace file@(_, fileSize) cache
    | file `C.fits` cache = cache
    | sizeT1 > max 1 (p cache) || (sizeOfT1 cache + fileSize) > maxSize cache = replace file $ fromT1ToB1 cache
    | otherwise = replace file $ fromT2ToB2 cache
    where
        sizeT1 = fromIntegral $ 1 + Clock.elements (t1 cache)

replaceGhostCaches :: Bool -> File -> Car -> Car
replaceGhostCaches True _ cache = cache
replaceGhostCaches inGhostCache file@(_, fileSize) cache
    | size_1 > maxCacheSize = rerunWith cache {b1 = Lru.removeLRU $ b1 cache}
    | size_2 > (2 * maxCacheSize) = rerunWith cache {b2 = Lru.removeLRU $ b2 cache}
    | otherwise = cache
    where
        size_1 = Clock.size (t1 cache) + C.size (b1 cache) + fileSize
        size_2 = size_1 + Clock.size (t2 cache) + C.size (b2 cache)
        maxCacheSize = maxSize cache
        rerunWith = replaceGhostCaches inGhostCache file

fromT1ToB1 :: Car -> Car
fromT1ToB1 cache =
    let t1Clock = t1 cache
        (removedFile, t1') = Clock.tick t1Clock
        b1' = removedFile `C.to` b1 cache
    in cache {t1 = t1', b1 = b1'}

fromT2ToB2 :: Car -> Car
fromT2ToB2 cache =
    let t2Clock = t2 cache
        (removedFile, t2') = Clock.tick t2Clock
        b2' = removedFile `C.to` b2 cache
    in cache {t2 = t2', b2 = b2'}

incP :: Car -> Car
incP cache = updateP' (+) (C.size $ b2 cache) (C.size $ b1 cache) cache

decP :: Car -> Car
decP cache = updateP' (-) (C.size $ b1 cache) (C.size $ b2 cache) cache

remove :: File -> Car -> Car
remove file cache =
    let (inT1, t1') = file `Clock.remove` t1 cache
        (inT1OrT2, t2') = if not inT1 then file `Clock.remove` t2 cache else (False, t2 cache)
        (inB1OrRest, b1') = if not inT1OrT2 then file `Lru.removeMaybe` b1 cache else (False, b1 cache)
        (_, b2') = if not inB1OrRest then file `Lru.removeMaybe` b2 cache else (False, b2 cache)
    in cache {t1 = t1', t2 = t2', b1 = b1', b2 = b2'}
