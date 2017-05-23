module Clock.Cart

where

import           Data.Maybe       (fromJust, isJust, isNothing)

import           Cache            (CacheSize, File, WriteStrategy, size)
import qualified Cache            as C (Cache (..), to)
import qualified Clock.CartClock  as CC
import           Clock.ClockCache
import qualified Lru.LruHash      as Lru
import           Util.MathUtil    (multipliedBy)

data Cart = Cart { t1            :: CC.CartClock
                 , b1            :: Lru.Lru
                 , t2            :: CC.CartClock
                 , b2            :: Lru.Lru
                 , p             :: Double
                 , q             :: CacheSize -- target size of b1
                 , sizeShortTerm :: CacheSize
                 , sizeLongTerm  :: CacheSize
                 , maxSize       :: CacheSize
                 , writeStrategy :: WriteStrategy
                 }

instance C.Cache Cart where
    size cache = CC.size (t1 cache) + CC.size (t2 cache)
    maxSize = maxSize
    remove = remove
    readFromCache = readFromCache
    to = to
    empty maxCacheSize ws = Cart { t1 = CC.empty
                                            , t2 = CC.empty
                                            , b1 = C.empty maxCacheSize ws
                                            , b2 = C.empty maxCacheSize ws
                                            , p = 0.0
                                            , q = 0
                                            , sizeShortTerm = 0
                                            , sizeLongTerm = 0
                                            , maxSize = maxCacheSize
                                            , writeStrategy = ws
                                            }
    writeStrategy = writeStrategy

instance ClockCache Cart where
    getP = p
    setP p' cache = cache {p = p'}
    sizeOfT1 = CC.size . t1
    sizeOfT2 = CC.size . t2

readFromCache :: File -> Cart -> (Bool, Cart)
readFromCache file cache =
    let (inT1, updatedT1) = file `CC.inCache` t1 cache
        (inT2, updatedT2) = if not inT1 then file `CC.inCache` t2 cache else (False, t2 cache)
    in if inT1 || inT2
        then (True, cache {t1 = updatedT1, t2 = updatedT2})
        else (False, file `to` cache)

to :: File -> Cart -> Cart
to file cache
    | not inGhostCache = file `asShortTermToT1` preparedCache
    | inB1 = fromB1ToT1 file . incP $ preparedCache
    | otherwise = incQ . fromB2ToT1 file . decP $ preparedCache
    where
        inB1 = file `Lru.inCache` b1 cache
        inB2 = file `Lru.inCache` b2 cache
        inGhostCache = inB1 || inB2
        preparedCache = replaceGhostCaches inGhostCache $ replace file cache

asShortTermToT1 :: File -> Cart -> Cart
asShortTermToT1 file cache = cache { t1 = file `CC.asShortTermTo` t1 cache
                                   , sizeShortTerm = newShortTermSize file cache
                                   }

asLongTermToT1 :: File -> Cart -> Cart
asLongTermToT1 file cache = cache { t1 = file `CC.asLongTermTo` t1 cache
                                  , sizeShortTerm = newLongTermSize file cache
                                  }

fromB1ToT1 :: File -> Cart -> Cart
fromB1ToT1 file cache =
    let cache' = file `asLongTermToT1` cache
    in cache' {b1 = file `C.remove` b1 cache'}

fromB2ToT1 :: File -> Cart -> Cart
fromB2ToT1 file cache =
    let cache' = file `asLongTermToT1` cache
    in cache' {b2 = file `C.remove` b2 cache'}


newShortTermSize :: File -> Cart -> CacheSize
newShortTermSize (_, fileSize) cache =
    let currentShortTermSize = sizeShortTerm cache
    in currentShortTermSize + fileSize

newLongTermSize :: File -> Cart -> CacheSize
newLongTermSize (_, fileSize) cache =
    let currentLongTermSize = sizeLongTerm cache
    in currentLongTermSize + fileSize

replace :: File -> Cart -> Cart
replace file cache
    | not $ cache `toSmallFor` file = cache -- ok
    | otherwise = replace file cache'''
    where
        (t2', t1') = CC.pushToOtherWhileReferenced (t2 cache) (t1 cache)
        cache' = incQ cache {t1 = t1', t2 = t2'}
        cache'' = prepareT1 cache'
        cache''' = cache'' `freeFor` file

freeFor:: Cart -> File -> Cart
freeFor cache file@(_, fileSize)
    | not cacheTooSmall = cache
    | sizeT1 + fileSize > pInBytes cache = fromT1ToB1 cache
    | otherwise = fromT2ToB2 cache
    where cacheTooSmall = cache `toSmallFor` file
          sizeT1 = CC.size . t1 $ cache

incQ :: Cart -> Cart
incQ cache =
    let sizeT1 = CC.size . t1 $ cache
        sizeT2 = CC.size . t2 $ cache
        sizeB2 = size . b2 $ cache
        sizeShortTermFiles = sizeShortTerm cache
        c = maxSize cache
        currentQ = q cache
    in if sizeT2 + sizeB2 + sizeT1 - sizeShortTermFiles >= c
        then cache {q = min (currentQ + pseudoPageSizeInBytes cache) (2 * c - sizeT1)}
        else cache

decQ :: Cart -> Cart
decQ cache =
    let sizeT1 = CC.size . t1 $ cache
        c = maxSize cache
        currentQ = q cache
    in cache {q = max (currentQ - pseudoPageSizeInBytes cache) (c - sizeT1)}

prepareT1 :: Cart -> Cart
prepareT1 cache =
    let (fileToMove, t1') = CC.getLongTermOrReferencedHead $ t1 cache
        Just (fileId, (referenceBit, filterBit, fileSize)) = fileToMove
        sizeB1 = size . b1 $ cache
        fileShouldBeLongterm = sizeOfT1 cache >= min sizeB1 ((p cache + pseudoPageSize) `multipliedBy` maxSize cache) && filterBit == CC.ShortTerm
        sizeShortTermFiles = sizeShortTerm cache
        sizeLongTermFiles = sizeLongTerm cache
        file = (fileId, fileSize)
    in if isNothing fileToMove
        then cache
        else if referenceBit == Referenced
            then
                if fileShouldBeLongterm then
                    prepareT1 cache {t1 = file `CC.asLongTermTo` t1', sizeShortTerm = sizeShortTermFiles - fileSize, sizeLongTerm = sizeLongTermFiles + fileSize}
                else
                    prepareT1 cache {t1 = CC.to file filterBit t1'}
            else
                prepareT1 . decQ $ cache {t1 = t1', t2 = file `CC.asLongTermTo` t2 cache}


replaceGhostCaches :: Bool -> Cart -> Cart
replaceGhostCaches True cache = cache
replaceGhostCaches _ cache
    | ghostListTooBig && (sizeB2 == 0 || sizeB1 > q cache) = replaceGhostCaches False cache {b1 = Lru.removeLRU $ b1 cache}
    | ghostListTooBig = replaceGhostCaches False cache {b2 = Lru.removeLRU $ b2 cache}
    | otherwise = cache
    where
        sizeB1 = C.size . b1 $ cache
        sizeB2 = C.size . b2 $ cache
        ghostListSize = sizeB1 + sizeB2
        ghostListTooBig = ghostListSize > maxSize cache

fromT1ToB1 :: Cart -> Cart
fromT1ToB1 cache =
    let t1Clock = t1 cache
        (removedFile@(_, fileSize), t1') = CC.tick t1Clock
        b1' = removedFile `C.to` b1 cache
    in cache {t1 = t1', b1 = b1', sizeShortTerm = sizeShortTerm cache - fileSize}

fromT2ToB2 :: Cart -> Cart
fromT2ToB2 cache =
    let t2Clock = t2 cache
        (removedFile@(_, fileSize), t2') = CC.tick t2Clock
        b2' = removedFile `C.to` b2 cache
    in cache {t2 = t2', b2 = b2', sizeLongTerm = sizeLongTerm cache - fileSize}

incP :: Cart -> Cart
incP cache = updateP (+) (sizeShortTerm cache) (C.size $ b1 cache) cache

decP :: Cart -> Cart
decP cache = updateP (-) (sizeLongTerm cache) (C.size $ b2 cache) cache

remove :: File -> Cart -> Cart
remove file@(_, fileSize) cache =
    let (removedFileT1, t1') = file `CC.removeAndGetFilterBit` t1 cache
        (removedFile, t2') = if isNothing removedFileT1 then file `CC.removeAndGetFilterBit` t2 cache else (removedFileT1, t2 cache)
        (inB1OrRest, b1') = if isNothing removedFile then file `Lru.removeMaybe` b1 cache else (False, b1 cache)
        (_, b2') = if not inB1OrRest then file `Lru.removeMaybe` b2 cache else (False, b2 cache)
        filterBit = fromJust removedFile
        sizeShortTerm' = sizeShortTerm cache - (if isJust removedFile && filterBit == CC.LongTerm then 0 else fileSize)
        sizeLongTerm' = sizeShortTerm cache - (if isJust removedFile && filterBit == CC.LongTerm then fileSize else 0)
    in cache {t1 = t1', t2 = t2', b1 = b1', b2 = b2', sizeShortTerm = sizeShortTerm', sizeLongTerm = sizeLongTerm'}
