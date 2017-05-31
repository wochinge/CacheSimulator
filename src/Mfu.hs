module Mfu
    ( Mfu(..)
    ) where
import qualified Cache        as C
import qualified Data.HashPSQ as H (HashPSQ, alter, alterMin, empty, insert)
import           Data.Maybe   (isJust)
import           Request
import           SimpleCaches (readFromCache')

type FilePrio = Int
data Mfu = Mfu { files         :: H.HashPSQ FileID FilePrio FileSize
               , size          :: C.CacheSize
               , maxSize       :: C.CacheSize
               , writeStrategy :: C.WriteStrategy
               }

instance C.Cache Mfu where
    to = insert'
    readFromCache = readFromCache' updateCache
    remove = remove'
    empty = Mfu H.empty 0
    size = size
    maxSize = maxSize
    writeStrategy = writeStrategy

updateCache :: C.File -> Mfu -> (Bool, Mfu)
updateCache (fileID, _) cache =
    let (itemWasInCache, files') = H.alter updateItem fileID $ files cache
    in (itemWasInCache, cache {files = files'})

updateItem ::  Maybe (FilePrio, FileSize) -> (Bool, Maybe (FilePrio, FileSize))
updateItem Nothing                 = (False, Nothing)
updateItem (Just (prio, fileSize)) = (True, Just (prio + 1, fileSize))

insert' :: C.File -> Mfu -> Mfu
insert' f@(fileID, fileSize) cache
    | f `C.biggerAsMax` cache = cache
    | C.fits f cache = cache {files = files', size = size cache + fileSize}
    | otherwise = insert' f (removeLFU cache)
    where files' = H.insert fileID 1 fileSize $ files cache

removeLFU :: Mfu -> Mfu
removeLFU cache =
    let (sizeOfRemoved, files') = H.alterMin delete' $ files cache
    in cache {files = files', size = size cache - sizeOfRemoved}

delete' :: Maybe (FileID, FilePrio, FileSize) -> (FileSize, Maybe (FileID, FilePrio, FileSize))
delete' Nothing                 = (0, Nothing)
delete' (Just (_, _, fileSize)) = (fileSize, Nothing)

remove' :: C.File -> Mfu -> Mfu
remove' (fileID, fileSize) cache =
    let (removed, updatedFiles) = H.alter (\f -> (isJust f, Nothing)) fileID $ files cache
        currentSize = size cache
        newSize = if removed then currentSize - fileSize else currentSize
    in cache {files = updatedFiles, size = newSize}
