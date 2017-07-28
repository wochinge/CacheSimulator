module Lfu
    ( Lfu(..)
    ) where
import qualified Cache        as C
import qualified Data.HashPSQ as H (HashPSQ, alter, alterMin, empty, insert)
import           Data.Maybe   (isJust)
import           Request
import           SimpleCaches (readFromCache')

type FilePrio = Int
data Lfu = Lfu { files         :: H.HashPSQ FileID FilePrio FileSize
               , size          :: C.CacheSize
               , maxSize       :: C.CacheSize
               , writeStrategy :: C.WriteStrategy
               }

instance C.Cache Lfu where
    to = insert'
    readFromCache = readFromCache' updateCache
    remove = remove'
    empty = Lfu H.empty 0
    size = size
    maxSize = maxSize
    writeStrategy = writeStrategy

updateCache :: C.File -> Lfu -> (Bool, Lfu)
updateCache (fileID, _) cache =
    let (itemWasInCache, files') = H.alter updateItem fileID $ files cache
    in (itemWasInCache, cache {files = files'})

updateItem ::  Maybe (FilePrio, FileSize) -> (Bool, Maybe (FilePrio, FileSize))
updateItem Nothing                 = (False, Nothing)
updateItem (Just (prio, fileSize)) = (True, Just (prio + 1, fileSize))

insert' :: C.File -> Lfu -> Lfu
insert' f@(fileID, fileSize) cache
    | f `C.biggerAsMax` cache = cache
    | C.fits f cache = cache {files = files', size = size cache + fileSize}
    | otherwise = insert' f (removeLFU cache)
    where files' = H.insert fileID 1 fileSize $ files cache

removeLFU :: Lfu -> Lfu
removeLFU cache =
    let (sizeOfRemoved, files') = H.alterMin delete' $ files cache
    in cache {files = files', size = size cache - sizeOfRemoved}

delete' :: Maybe (FileID, FilePrio, FileSize) -> (FileSize, Maybe (FileID, FilePrio, FileSize))
delete' Nothing                 = (0, Nothing)
delete' (Just (_, _, fileSize)) = (fileSize, Nothing)

remove' :: C.File -> Lfu -> Lfu
remove' (fileID, fileSize) cache =
    let (removed, updatedFiles) = H.alter (\f -> (isJust f, Nothing)) fileID $ files cache
        currentSize = size cache
        newSize = if removed then currentSize - fileSize else currentSize
    in cache {files = updatedFiles, size = newSize}
