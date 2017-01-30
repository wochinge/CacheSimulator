{-# LANGUAGE FlexibleInstances #-}
module Mfu
    ( Mfu
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
type Mfu = (H.HashPSQ FileID FilePrio FileSize, CacheSize, CacheSize)

instance Cache Mfu where
    to = insert'
    update = updateCache'
    remove = remove'
    empty maxCacheSize = (H.empty, 0, maxCacheSize)
    size (_, size, _) = size
    maxSize (_, _, maxSize) = maxSize

updateCache' :: File -> Mfu -> (Bool, Mfu)
updateCache' f@(fileID, fileSize) c@(cachedFiles, cacheSize, maxCacheSize) =
    let (itemWasInCache, alteredFiles) = H.alter updateItem fileID cachedFiles
    in (itemWasInCache, (alteredFiles, cacheSize, maxCacheSize))

updateItem ::  Maybe (FilePrio, FileSize) -> (Bool, Maybe (FilePrio, FileSize))
updateItem Nothing = (False, Nothing)
updateItem (Just (prio, size)) = (True, Just (prio + 1, size))

insert' :: File -> Mfu -> Mfu
insert' f@(fileID, fileSize) cache@(cachedFiles, cacheSize, maxCacheSize)
    | fits f cache = (H.insert fileID 1 fileSize cachedFiles, cacheSize + fileSize, maxCacheSize)
    | otherwise = insert' f (removeLFU cache)

removeLFU :: Mfu -> Mfu
removeLFU (cachedFiles, cacheSize, maxCacheSize) =
    let (sizeOfRemoved, updated) = H.alterMin delete' cachedFiles
    in (updated, cacheSize - sizeOfRemoved, maxCacheSize)

delete' :: Maybe (FileID, FilePrio, FileSize) -> (FileSize, Maybe (FileID, FilePrio, FileSize))
delete' Nothing = (0, Nothing)
delete' (Just (_, _, size)) = (size, Nothing)

remove' :: File -> Mfu -> Mfu
remove' (fileID, fileSize) (cachedFiles, cacheSize, maxSize) =
    let (removed, updatedFiles) = H.alter (\f -> (isJust f, Nothing)) fileID cachedFiles
        newSize = if removed then cacheSize - fileSize else cacheSize
    in (updatedFiles, newSize, maxSize)
