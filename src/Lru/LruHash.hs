module Lru.LruHash
    ( Lru(..)
    , removeMaybe
    , inCache
    , removeLRU
    , removeAndRetrieveLRU
    , updateCache
    ) where

import qualified Cache        as C
import qualified Data.HashPSQ as H (HashPSQ, alter, empty, insert, member,
                                    minView)
import           Request
import           SimpleCaches (readFromCache')

type FilePrio = Int

data Lru = Lru { files         :: H.HashPSQ FileID FilePrio FileSize
               , nextPrio      :: FilePrio
               , size          :: C.CacheSize
               , maxSize       :: C.CacheSize
               , writeStrategy :: C.WriteStrategy
               }

instance C.Cache Lru where
    to = to'
    readFromCache = readFromCache' updateCache
    remove file cache = snd $ removeMaybe file cache
    empty = Lru H.empty 0 0
    size = size
    maxSize = maxSize
    writeStrategy = writeStrategy

updateCache :: C.File -> Lru -> (Bool, Lru)
updateCache (fileID, _) cache =
    let (itemWasInCache, alteredFiles) = H.alter (updateItem prio) fileID cachedFiles
        cachedFiles = files cache
        prio = nextPrio cache
    in (itemWasInCache, cache {files = alteredFiles, nextPrio = prio + 1})

updateItem ::  FilePrio -> Maybe (FilePrio, FileSize) -> (Bool, Maybe (FilePrio, FileSize))
updateItem _ Nothing                    = (False, Nothing)
updateItem newPrio (Just (_, fileSize)) = (True, Just (newPrio, fileSize))

to' :: C.File -> Lru -> Lru
to' f@(fileID, fileSize) cache
    | f `C.biggerAsMax` cache = cache
    | C.fits f cache = cache {files = files', nextPrio = prio + 1, size = currentCacheSize + fileSize}
    | otherwise = to' f (removeLRU cache)
    where prio = nextPrio cache
          currentCacheSize = size cache
          files' = H.insert fileID prio fileSize $ files cache

removeLRU :: Lru -> Lru
removeLRU cache = snd $ removeAndRetrieveLRU cache

removeAndRetrieveLRU :: Lru -> (C.File, Lru)
removeAndRetrieveLRU cache =
    let removeResult = H.minView $ files cache
        currentSize = size cache
    in case removeResult of
        Just (fileId, _, sizeOfRemoved, files') -> ((fileId, sizeOfRemoved), cache {files = files', size = currentSize - sizeOfRemoved})
        _ -> error "The emptiness check should have been performed before"

removeMaybe :: C.File -> Lru -> (Bool, Lru)
removeMaybe (fileID, _) cache =
    let (removed, files') = H.alter (\f -> (f, Nothing)) fileID $ files cache
        currentSize = size cache
    in case removed of
        Just (_, sizeOfRemoved) -> (True, cache {files = files', size = currentSize - sizeOfRemoved})
        _ -> (False, cache)

inCache :: C.File -> Lru -> Bool
inCache (filedID, _) cache = filedID `H.member` files cache
