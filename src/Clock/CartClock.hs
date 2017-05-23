module Clock.CartClock
( CartClock (..)
, empty
, inCache
, to
, tick
, asShortTermTo
, asLongTermTo
, removeAndGetFilterBit
, pushToOtherWhileReferenced
, getLongTermOrReferencedHead
, FilterBit (..)
) where

import           Cache            (CacheSize, File)
import           Clock.ClockCache (PageReferenceBit (..))
import qualified Data.HashPSQ     as H (HashPSQ, alter, alterMin, deleteView,
                                        empty, insert, minView)
import           Data.Maybe       (fromJust)
import           Request          (FileID, FileSize)

type FilePrio = Integer
data FilterBit = ShortTerm | LongTerm deriving (Show, Eq)

type ClockFile = (PageReferenceBit, FilterBit, FileSize)
type ClockFileId = (FileID, ClockFile)

data CartClock = CartClock { files    :: H.HashPSQ FileID FilePrio ClockFile
                           , nextPrio :: FilePrio
                           , size     :: CacheSize
                           } deriving (Show, Eq)

empty :: CartClock
empty = CartClock H.empty 0 0

inCache :: File -> CartClock -> (Bool, CartClock)
inCache (fileID, _) clock =
    let (isInCache, updatedFiles) = H.alter updatePageReference fileID $ files clock
    in (isInCache, clock {files = updatedFiles})

updatePageReference :: Maybe (FilePrio, ClockFile) -> (Bool, Maybe (FilePrio, ClockFile))
updatePageReference Nothing = (False, Nothing)
updatePageReference (Just (filePrio, (_, filterBit, fileSize))) = (True, Just (filePrio, (Referenced, filterBit, fileSize)))

tick :: CartClock -> (File, CartClock)
tick clock =
    let cached = files clock
        currentSize = size clock
        removeResult = H.minView cached
    in case removeResult of
        Just (fileId, _, (_, _, fileSize), files') -> ((fileId, fileSize), clock {files = files', size = currentSize - fileSize})
        _ -> error "Should not happen"

pushToOtherWhileReferenced :: CartClock -> CartClock -> (CartClock, CartClock)
pushToOtherWhileReferenced self other =
    let (removed, clockFiles) = H.alterMin deleteAndGetIfReference $ files self
        (fileID, (_, filterBit, fileSizeRemoved)) = fromJust removed
        other' = to (fileID, fileSizeRemoved) filterBit other
    in case removed of
        Nothing -> (self, other)
        Just _ -> pushToOtherWhileReferenced self {files = clockFiles, size = size self - fileSizeRemoved} other'

deleteAndGetIfReference :: Maybe (FileID, FilePrio, ClockFile) -> (Maybe ClockFileId, Maybe (FileID, FilePrio, ClockFile))
deleteAndGetIfReference Nothing = (Nothing, Nothing)
deleteAndGetIfReference file@(Just (_, _, (NotReferenced, _, _))) = (Nothing, file)
deleteAndGetIfReference (Just (fileId, _, (_, filterBit, fileSize))) = (Just (fileId, (NotReferenced, filterBit, fileSize)), Nothing)

getLongTermOrReferencedHead :: CartClock -> (Maybe ClockFileId, CartClock)
getLongTermOrReferencedHead clock =
    let (removed, files') = H.alterMin deleteAndRetrieveLongTermOrReferenced $ files clock
    in case removed of
        Nothing -> (Nothing, clock)
        Just (_, (_, _, fileSize)) -> (removed, clock {files = files', size = size clock - fileSize})

deleteAndRetrieveLongTermOrReferenced :: Maybe (FileID, FilePrio, ClockFile) -> (Maybe (FileID, ClockFile), Maybe (FileID, FilePrio, ClockFile))
deleteAndRetrieveLongTermOrReferenced Nothing = (Nothing, Nothing)
deleteAndRetrieveLongTermOrReferenced file@(Just (fileId, _, (referenced, filterBit, fileSize)))
    | referenced == Referenced || filterBit == LongTerm = (Just (fileId, (NotReferenced, filterBit, fileSize)), Nothing)
    | otherwise = (Nothing, file)

removeAndGetFilterBit :: File -> CartClock -> (Maybe FilterBit, CartClock)
removeAndGetFilterBit (fileID, _) clock =
    let currentSize = size clock
        removeResult = H.deleteView fileID $ files clock
    in case removeResult of
        Just (_, (_, filterBit, fileSize), newFiles) -> (Just filterBit, clock {files = newFiles, size = currentSize - fileSize})
        _ -> (Nothing, clock)

asShortTermTo :: File -> CartClock -> CartClock
asShortTermTo file = to file ShortTerm

asLongTermTo :: File -> CartClock -> CartClock
asLongTermTo file = to file LongTerm

to :: File -> FilterBit -> CartClock -> CartClock
to (fileID, fileSize) filterBit clock =
    let prio = nextPrio clock
        newFiles = H.insert fileID prio (NotReferenced, filterBit, fileSize) $ files clock
        currentSize = size clock
    in clock {files = newFiles, nextPrio = prio + 1, size = currentSize + fileSize}
