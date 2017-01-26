{-# LANGUAGE FlexibleInstances #-}
module LruHash
    ( Lru
    , to
    , remove
    , empty
    ) where
import Prelude hiding (List(..))
import System.IO
import qualified Data.HashPSQ as H (HashPSQ(..), insert, empty, alter, alterMin, delete)
import Request
import Cache
import Data.Maybe(isJust)

type FilePrio = Int
type Lru = (H.HashPSQ FileID FilePrio FileSize, FilePrio, CacheSize, CacheSize)

instance Cache Lru where
    to = insert'
    update = updateCache'
    remove (filedID, fileSize) (cachedFiles, prio, cacheSize, maxCacheSize) =
        (H.delete filedID cachedFiles, prio, cacheSize - fileSize, maxCacheSize)
    empty maxCacheSize = (H.empty, 0, 0, maxCacheSize)
    size (_, _, size, _) = size
    maxSize (_, _, _, maxSize) = maxSize

updateCache' :: File -> Lru -> (Bool, Lru)
updateCache' f@(fileID, fileSize) c@(cachedFiles, prio, cacheSize, maxCacheSize) =
    let (itemWasInCache, alteredFiles) = H.alter (updateItem prio) fileID cachedFiles
        newPrio = if itemWasInCache then prio + 1 else prio
    in (itemWasInCache, (alteredFiles, newPrio, cacheSize, maxCacheSize))

updateItem ::  FilePrio -> Maybe (FilePrio, FileSize) -> (Bool, Maybe (FilePrio, FileSize))
updateItem _ Nothing = (False, Nothing)
updateItem newPrio (Just (prio, size)) = (True, Just (newPrio, size))

insert' :: File -> Lru -> Lru
insert' f@(fileID, fileSize) cache@(cachedFiles, prio, cacheSize, maxCacheSize)
    | fits fileSize cache = (H.insert fileID prio fileSize cachedFiles, prio + 1, cacheSize + fileSize, maxCacheSize)
    | otherwise = insert' f (removeLRU cache)

removeLRU :: Lru -> Lru
removeLRU (cachedFiles, prio, cacheSize, maxCacheSize) =
    let (sizeOfRemoved, updated) = H.alterMin delete' cachedFiles
    in (updated, prio, cacheSize - sizeOfRemoved, maxCacheSize)

delete' :: Maybe (FileID, FilePrio, FileSize) -> (FileSize, Maybe (FileID, FilePrio, FileSize))
delete' Nothing = (0, Nothing)
delete' (Just (_, _, size)) = (size, Nothing)
