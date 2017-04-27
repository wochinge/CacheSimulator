
module Clock where

import Data.Maybe (isJust)
import qualified Data.HashPSQ as H (HashPSQ(..), insert, empty, alter, alterMin, delete, member)

import Request (FileSize, FileID)
import Cache (CacheSize, File)

type FilePrio = Int
data PageReferenceBit = Referenced | NotReferenced deriving (Show, Eq)
type ClockFile = (PageReferenceBit, FileSize)
data Clock = Clock { files :: H.HashPSQ FileID FilePrio ClockFile
                   , nextPrio :: FilePrio
                   , size :: CacheSize
                   } deriving (Show, Eq)

empty :: Clock
empty = Clock H.empty 0 0

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
        Just file@(fileID, fileSize) -> (file, clock {files = updatedFiles, size = currentSize - fileSize})

checkPageReference :: FilePrio -> Maybe (FileID, FilePrio, ClockFile) -> (Maybe File, Maybe (FileID, FilePrio, ClockFile))
checkPageReference _ Nothing = (Nothing, Nothing)
checkPageReference nextPrio (Just (fileID, prio, (referenced, size)))
    | referenced == Referenced = (Nothing, Just (fileID, nextPrio, (NotReferenced, size)))
    | otherwise = (Just (fileID, size), Nothing)

remove :: File -> Clock -> (Bool, Clock)
remove (fileID, fileSize) clock =
    let cachedFiles = files clock
        currentSize = size clock
        remove = maybe (False, Nothing) (const (True, Nothing))
        (fileRemoved, newFiles) = H.alter remove fileID cachedFiles
    in if fileRemoved
        then (True, clock {files = newFiles, size = currentSize - fileSize})
        else (False, clock)

to :: File -> Clock -> Clock
to (fileID, fileSize) clock =
    let prio = nextPrio clock
        newFiles = H.insert fileID prio (NotReferenced, fileSize) $ files clock
        currentSize = size clock
    in clock {files = newFiles, nextPrio = prio + 1, size = currentSize + fileSize}
