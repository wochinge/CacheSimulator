{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Clock.ClockCache
( ClockCache(..)
, pseudoPageSize
, pseudoPageSizeInBytes
, pInBytes
, updateP
, updateP'
, toSmallFor
, PageReferenceBit (..)
) where

import           Cache         (Cache, CacheSize, File, maxSize)
import           Util.MathUtil (multipliedBy, toBounds)

class Cache a => ClockCache a where
    getP :: a -> Double
    setP :: Double -> a -> a
    sizeOfT1 :: a -> CacheSize
    sizeOfT2 :: a -> CacheSize

data PageReferenceBit = Referenced | NotReferenced deriving (Show, Eq)

pInBytes :: ClockCache a => a -> CacheSize
pInBytes cache =
    let p = getP cache
        cacheSize = maxSize cache
    in max (pseudoPageSizeInBytes cache) (p `multipliedBy` cacheSize)

pseudoPageSize :: Double
pseudoPageSize = 0.01

pseudoPageSizeInBytes :: ClockCache a => a -> CacheSize
pseudoPageSizeInBytes cache = pseudoPageSize `multipliedBy` maxSize cache

updateP :: ClockCache a => (Double -> Double -> Double) -> CacheSize -> CacheSize -> a -> a
updateP minusOrPlus numeratorSize denominatorSize cache =
    let oldP = getP cache
        pageSize = pseudoPageSize
        numeratorSize' = fromIntegral numeratorSize
        denominatorSize' = if denominatorSize == 0 then error "Denominator should not be zero" else fromIntegral denominatorSize
        delta = pageSize * max 1 (numeratorSize' / denominatorSize')
        p' = (oldP `minusOrPlus` delta) `toBounds` (0, 1)
    in setP p' cache

updateP' :: ClockCache a => (Double -> Double -> Double) -> CacheSize -> CacheSize -> a -> a
updateP' minusOrPlus numeratorSize denominatorSize cache =
    let oldP = getP cache
        numeratorSize' = fromIntegral numeratorSize
        denominatorSize' = fromIntegral denominatorSize
        delta = max 1 (numeratorSize' / denominatorSize')
        p' =  if sizeOfT1 cache > maxSize cache
                then oldP - 1
                else fromIntegral . round $ max 0.0 (oldP `minusOrPlus` delta)
    in setP p' cache

toSmallFor :: ClockCache a => a -> File -> Bool
toSmallFor cache (_, fileSize) =
    let sizeT1 = sizeOfT1 cache
        sizeT2 = sizeOfT2 cache
    in sizeT1 + sizeT2 + fileSize > maxSize cache
