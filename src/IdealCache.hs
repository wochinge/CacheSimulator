
module IdealCache
( IdealCache (..)
, getIdealCacheStatistic
) where

import qualified Cache         as C
import qualified Data.HashPSQ  as H (HashPSQ, alter, delete, empty, insert,
                                     insert, lookup, member, minView)
import           Data.Maybe    (fromJust, isJust)
import           Data.Sequence (ViewL (..), ViewR (..))
import qualified Data.Sequence as S
import           Request       (FileID, FileRequest, FileSize, RequestType (..),
                                forEachFileRequestIn)

type FilePrio = Int
data Version = Same | Changed deriving Eq
data Empty = Empty

type OtherRequest = (Version, Maybe FilePrio)

data IdealCache = IdealCache { futureCached :: H.HashPSQ FileID FilePrio FileSize
                             , size :: C.CacheSize
                             , maxSize :: C.CacheSize
                             , future :: H.HashPSQ FileID FilePrio (S.Seq OtherRequest)
                             , writeStrategy :: C.WriteStrategy
                             }

instance C.Cache IdealCache where
    size = size
    maxSize = maxSize
    writeStrategy = writeStrategy
    readFromCache = readFromCache
    remove = remove
    to = to
    empty maxCacheSize = IdealCache H.empty 0 maxCacheSize H.empty

getIdealCacheStatistic :: IdealCache -> String -> IO C.CacheStatistic
getIdealCacheStatistic cache logFileName =
    let f rs = C.calculateHits rs ((0, 0), initFuture rs 100000000 cache)
    in f `forEachFileRequestIn` logFileName

readFromCache :: C.File -> IdealCache -> (Bool, IdealCache)
readFromCache f@(fileId, _) cache
    | inCache = (True, updateFuture f cache)
    | otherwise = (False, f `to` cache)
    where inCache = H.member fileId $ futureCached cache

remove :: C.File -> IdealCache -> IdealCache
remove (fileId, fileSize) cache =
    let (_, future') = H.alter switchToNewFileVersion fileId $ future cache
    in cache { futureCached =  H.delete fileId $ futureCached cache
             , size = size cache - fileSize
             , future = future'
             }

to :: C.File -> IdealCache -> IdealCache
to file@(fileId, fileSize) cache
    | file `C.biggerAsMax` cache = cache
    | file `C.fits` cache = cache { size = size cache + fileSize
                                  , futureCached = H.insert fileId (getPrio file cache) fileSize $ futureCached cache
                                  }
    | otherwise = file `to` free cache

free :: IdealCache -> IdealCache
free cache =
    let Just (fileId, _, fileSize, futureCached') = H.minView $ futureCached cache
    in cache { futureCached = futureCached'
             , size = size cache - fileSize
             }

getPrio :: C.File -> IdealCache -> FilePrio
getPrio (fileId, _) cache =
    let futureOfFile = H.lookup fileId $ future cache
    in case futureOfFile of
        Just (prio, _) -> prio
        _              -> 0

updateFuture:: C.File -> IdealCache -> IdealCache
updateFuture f@(fileId, fileSize) cache =
    let (newPrio, cache') = currentRequestToPast f cache
    in cache' { futureCached = H.insert fileId newPrio fileSize $ futureCached cache }

currentRequestToPast :: C.File -> IdealCache -> (FilePrio, IdealCache)
currentRequestToPast (fileId, _) cache =
    let (prio, future') = H.alter updateRequest fileId $ future cache
    in (prio, cache { future = future' })

updateRequest :: Maybe (FilePrio, S.Seq OtherRequest) -> (FilePrio, Maybe (FilePrio, S.Seq OtherRequest))
updateRequest x@(Just (_, others))
    | S.null others = (0, Nothing) -- no more requests for this can, thus can be deleted
    | nextStatus == Changed = (0, Just(0, others))
    | otherwise = (prio, Just (prio, rest))
    where (nextStatus, maybePrio) :< rest = S.viewl others
          Just prio = maybePrio

-- Initial preparation of the future requests
initFuture :: [FileRequest] -> FilePrio -> IdealCache -> IdealCache
initFuture [] _ cache         = cache
initFuture (request@(_, fileId, _) : rest) prio cache =
    let (_, future') = H.alter (alter' request prio) fileId $ future cache
        nextPrio = prio - 1
    in initFuture rest nextPrio cache { future = future' }

alter' :: FileRequest -> FilePrio -> Maybe (FilePrio, S.Seq OtherRequest) -> (Empty, Maybe (FilePrio, S.Seq OtherRequest))
alter' (Read, _, _) prio Nothing = (Empty, Just (prio, S.empty))
alter' (Read, _, _) prio (Just (oldPrio, otherRequests)) = (Empty, Just (oldPrio, addToOtherRequests prio otherRequests))
alter' (_, _, _) _ Nothing = (Empty, Nothing)
alter' (_, _, _) _ (Just (prio, otherRequests)) = (Empty, Just (prio, addNewFileVersion otherRequests))

addToOtherRequests :: FilePrio -> S.Seq OtherRequest -> S.Seq OtherRequest
addToOtherRequests prio otherRequests
    | S.null otherRequests = S.singleton (Same, Just prio)
    | isJust prioOfLast = otherRequests S.|> (Same, Just prio)
    | otherwise = rest S.|> (Changed, Just prio)
    where rest :> (_, prioOfLast) = S.viewr otherRequests

addNewFileVersion :: S.Seq OtherRequest -> S.Seq OtherRequest
addNewFileVersion requests
    | S.null requests = S.empty
    | statusOfNext == Changed = requests
    | otherwise = requests S.|> (Changed, Nothing)
    where rest :> (statusOfNext, _) = S.viewr requests

switchToNewFileVersion :: Maybe (FilePrio, S.Seq OtherRequest) -> (Empty, Maybe (FilePrio, S.Seq OtherRequest))
switchToNewFileVersion Nothing = (Empty, Nothing)
switchToNewFileVersion v@(Just (_, others))
    | S.null others = (Empty, Nothing)
    | fileStatus == Changed && isJust maybePrio = (Empty, Just (prio, rest))
    | otherwise = error "Should not happen"
    where (fileStatus, maybePrio) :< rest = S.viewl others
          prio = fromJust maybePrio
