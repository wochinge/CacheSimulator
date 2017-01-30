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
    remove file cache = snd $ removeMaybe file cache
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
    | fits f cache = (H.insert fileID prio fileSize cachedFiles, prio + 1, cacheSize + fileSize, maxCacheSize)
    | otherwise = insert' f (removeLRU cache)

removeLRU :: Lru -> Lru
removeLRU (cachedFiles, prio, cacheSize, maxCacheSize) =
    let (sizeOfRemoved, updated) = H.alterMin delete' cachedFiles
    in (updated, prio, cacheSize - sizeOfRemoved, maxCacheSize)

delete' :: Maybe (FileID, FilePrio, FileSize) -> (FileSize, Maybe (FileID, FilePrio, FileSize))
delete' Nothing = (0, Nothing)
delete' (Just (_, _, size)) = (size, Nothing)

removeMaybe :: File -> Lru -> (Bool, Lru)
removeMaybe (fileID, fileSize) (cachedFiles, prio, cacheSize, maxSize) =
    let (removed, updatedFiles) = H.alter (\f -> (isJust f, Nothing)) fileID cachedFiles
        newSize = if removed then cacheSize - fileSize else cacheSize
    in (removed, (updatedFiles, prio, newSize, maxSize))
