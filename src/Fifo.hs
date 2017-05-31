module Fifo
( Fifo (..)
) where

import qualified Cache        as C
import qualified Data.HashPSQ as H (HashPSQ, deleteView, empty, insert, member,
                                    minView)
import           Request      (FileID, FileSize)

type FilePrio = Int

data Fifo = Fifo { files         :: H.HashPSQ FileID FilePrio FileSize
                 , prio          :: FilePrio
                 , size          :: C.CacheSize
                 , maxSize       :: C.CacheSize
                 , writeStrategy :: C.WriteStrategy
                 }

instance C.Cache Fifo where
    writeStrategy = writeStrategy
    remove = remove
    empty = Fifo H.empty 0 0
    maxSize = maxSize
    size = size
    to = to
    readFromCache = readFromCache

readFromCache :: C.File -> Fifo -> (Bool, Fifo)
readFromCache f@(fileId, _) cache
    | H.member fileId $ files cache = (True, cache)
    | otherwise = (False, f `to` cache)

to :: C.File -> Fifo -> Fifo
to f@(fileId, fileSize) cache
    | f `C.biggerAsMax` cache = cache
    | C.fits f cache = cache {files = files', prio = currentPrio + 1, size = size cache + fileSize}
    | otherwise = to f $ removeFirst cache
    where files' = H.insert fileId currentPrio fileSize $ files cache
          currentPrio = prio cache

removeFirst :: Fifo -> Fifo
removeFirst cache =
    let removeResult = H.minView $ files cache
    in case removeResult of
        Just (_, _, fileSize, files') -> cache {files = files', size = size cache - fileSize}
        _ -> error "Must be checked by layer above"

remove :: C.File -> Fifo -> Fifo
remove (fileId, _) cache =
    let removeResult = H.deleteView fileId $ files cache
    in case removeResult of
        Just (_, fileSize, files') -> cache {files = files', size = size cache - fileSize}
        _ -> cache
