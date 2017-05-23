module Clock.Clock where

import qualified Data.HashPSQ     as H (HashPSQ, alter, alterMin, deleteView,
                                        empty, insert)

import           Cache            (CacheSize, File)
import           Clock.ClockCache (PageReferenceBit (..))
import           Request          (FileID, FileSize)

type FilePrio = Int
type ClockFile = (PageReferenceBit, FileSize)
data Clock = Clock { files    :: H.HashPSQ FileID FilePrio ClockFile
                   , nextPrio :: FilePrio
                   , size     :: CacheSize
                   , elements :: CacheSize
                   } deriving (Show, Eq)

empty :: Clock
empty = Clock H.empty 0 0 0

inCache :: File -> Clock -> (Bool, Clock)
inCache (fileID, _) clock =
    let (isInCache, updatedFiles) = H.alter updatePageReference fileID $ files clock
    in (isInCache, clock {files = updatedFiles})

updatePageReference :: Maybe (FilePrio, ClockFile) -> (Bool, Maybe (FilePrio, ClockFile))
updatePageReference Nothing = (False, Nothing)
updatePageReference (Just (filePrio, (_, fileSize))) = (True, Just (filePrio, (Referenced, fileSize)))

tick :: Clock -> (File, Clock)
tick clock =
    let prio = nextPrio clock
        cached = files clock
        currentSize = size clock
        (removedFile, updatedFiles) = H.alterMin (checkPageReference prio) cached
    in case removedFile of
        Nothing -> tick clock {files = updatedFiles, nextPrio = prio + 1}
        Just file@(_, fileSize) -> (file, clock {files = updatedFiles, size = currentSize - fileSize, elements = elements clock - 1})

checkPageReference :: FilePrio -> Maybe (FileID, FilePrio, ClockFile) -> (Maybe File, Maybe (FileID, FilePrio, ClockFile))
checkPageReference _ Nothing = (Nothing, Nothing)
checkPageReference prio (Just (fileID, _, (referenced, fileSize)))
    | referenced == Referenced = (Nothing, Just (fileID, prio, (NotReferenced, fileSize)))
    | otherwise = (Just (fileID, fileSize), Nothing)

remove :: File -> Clock -> (Bool, Clock)
remove (fileId, _) clock =
    let cachedFiles = files clock
        removeResult = H.deleteView fileId cachedFiles
        currentSize = size clock
    in case removeResult of
        Just (_, (_, fileSize), cachedFiles') -> (True, clock {files = cachedFiles', size = currentSize - fileSize, elements = elements clock - 1})
        _ -> (False, clock)

to :: File -> Clock -> Clock
to (fileID, fileSize) clock =
    let prio = nextPrio clock
        newFiles = H.insert fileID prio (NotReferenced, fileSize) $ files clock
        currentSize = size clock
    in clock {files = newFiles, nextPrio = prio + 1, size = currentSize + fileSize, elements = elements clock + 1}
