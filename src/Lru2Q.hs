{-# LANGUAGE FlexibleInstances #-}
module Lru2Q
    ( Lru2Q(..)
    )

where

import qualified LruHash as Lru
import qualified Data.HashPSQ as H (HashPSQ(..), insert, empty, alter, alterMin, delete, member)
import Cache
import Request

type FilePrio = Int
type Lru2Q = (Lru.Lru, Lru.Lru, Lru.Lru, CacheSize)

instance Cache Lru2Q where
    empty = empty'
    readFromCache = update'
    to file cache@(am, ain, aout, maxSize) = (am, file `toAin` cache, aout, maxSize)
    remove = remove'
    size (am, ain, _, _) = size am + size ain
    maxSize (_, _, _, maxCacheSize) = maxCacheSize

empty' :: CacheSize -> Lru2Q
empty' maxCacheSize =
    let amList = empty maxCacheSize :: Lru.Lru
        ainList = empty maxCacheSize
        aoutList = empty $ maxCacheSize `div` 2
    in (amList, ainList, aoutList, maxCacheSize)

remove' :: File -> Lru2Q -> Lru2Q
remove' file (am, ain, aout, maxCacheSize) =
    let (wasInAmCache, updatedAm) = Lru.removeMaybe file am
        (wasInAinCache, updatedAin) = if not wasInAmCache then Lru.removeMaybe file ain else (True, ain)
        (_, updatedAout) = if not wasInAinCache then Lru.removeMaybe file aout else (True, aout)
    in (updatedAm, updatedAin, updatedAout, maxCacheSize)

update' :: File -> Lru2Q -> (Bool, Lru2Q)
update' file cache@(am, ain, aout, maxSize) =
    let (wasInAmCache, updatedAm) = file `Lru.updateCache` am
        (wasInAout, updatedAout) = if not wasInAmCache then file `Lru.removeMaybe` aout else (False, aout)
        updatedAm2 = if wasInAout then file `toAm` cache else updatedAm
        wasInAin = not wasInAout && file `Lru.inCache` ain
        updatedAin = if not wasInAin then file `toAin` cache else ain
    in (wasInAmCache || wasInAin, (updatedAm2, updatedAin, updatedAout, maxSize))

toAin :: File -> Lru2Q -> Lru.Lru
toAin file cache@(am, ain, aout, maxSize)
    | file `fits` cache = file `to` ain
    | otherwise = file `toAin` free2Q cache

toAm :: File -> Lru2Q -> Lru.Lru
toAm file cache@(am, ain, aout, maxSize)
    | file `fits` cache = file `to` am
    | otherwise = file `toAm` free2Q cache

free2Q :: Lru2Q -> Lru2Q
free2Q cache@(am, ain, aout, maxSize)
    | size ain > maxSize `div` 4 = (am, Lru.removeLRU ain, aout, maxSize)
    | otherwise = fromAmToAout cache

fromAmToAout :: Lru2Q -> Lru2Q
fromAmToAout (am, ain, aout, maxSize) =
    let (file, updatedAm) = Lru.removeAndRetrieveLRU am
        updatedAout = file `to` aout
    in (updatedAm, updatedAout, updatedAout, maxSize)
