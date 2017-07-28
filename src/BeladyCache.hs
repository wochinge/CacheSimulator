{-# LANGUAGE StrictData #-}
module BeladyCache
( BeladyCache (..)
, getBeladyCacheStatistic
, initFuture
) where

import qualified Cache         as C
import qualified Data.HashPSQ  as H (HashPSQ, alter, delete, deleteView, empty,
                                     insert, insert, lookup, member, minView)
import           Data.List     (foldl')
import           Data.Maybe    (fromJust, fromMaybe, isJust, isNothing)
import           Data.Sequence (ViewL (..), ViewR (..))
import qualified Data.Sequence as S
import           Debug.Trace   (trace)
import           Request       (FileID, FileRequest, FileSize, RequestType (..),
                                forEachFileRequestIn)
type FilePrio = Int
data Version = Same | Changed deriving (Eq, Show)
data Empty = Empty

type OtherRequest = (Version, Maybe FilePrio)

data BeladyCache = BeladyCache { futureCached :: H.HashPSQ FileID FilePrio FileSize
                             , size :: C.CacheSize
                             , maxSize :: C.CacheSize
                             , future :: H.HashPSQ FileID FilePrio (S.Seq OtherRequest)
                             , writeStrategy :: C.WriteStrategy
                             }

instance C.Cache BeladyCache where
    size = size
    maxSize = maxSize
    writeStrategy = writeStrategy
    readFromCache = readFromCache
    remove = remove
    to = to
    empty maxCacheSize = BeladyCache H.empty 0 maxCacheSize H.empty

getBeladyCacheStatistic :: BeladyCache -> String -> IO C.CacheStatistic
getBeladyCacheStatistic cache logFileName =
    let f rs = C.calculateHits rs ((0, 0), initFuture rs 100000000 cache)
    in f `forEachFileRequestIn` logFileName

readFromCache :: C.File -> BeladyCache -> (Bool, BeladyCache)
readFromCache f@(fileId, _) cache
    | inCache = (True, updateFuture f cache)
    | otherwise = (False, f `to` cache')
    where inCache = H.member fileId $ futureCached cache
          cache' = snd $ currentRequestToPast f cache

remove :: C.File -> BeladyCache -> BeladyCache
remove (fileId, _) cache =
    let (_, future') = H.alter switchToNewFileVersion fileId $ future cache
        removed = H.deleteView fileId $ futureCached cache
    in case removed of
        Just (_, fileSize, futureCached') -> cache { futureCached =  futureCached'
                                                        , size = size cache - fileSize
                                                        , future = future'
                                                        }
        _ -> cache { future = future' }

to :: C.File -> BeladyCache -> BeladyCache
to file@(fileId, fileSize) cache
    | file `C.biggerAsMax` cache = cache
    | file `C.fits` cache = cache { size = size cache + fileSize
                                  , futureCached = H.insert fileId (getPrio file cache) fileSize $ futureCached cache
                                  }
    | otherwise = file `to` free cache

free :: BeladyCache -> BeladyCache
free cache =
    let Just (fileId, _, fileSize, futureCached') = H.minView $ futureCached cache
    in cache { futureCached = futureCached'
             , size = size cache - fileSize
             }

getPrio :: C.File -> BeladyCache -> FilePrio
getPrio (fileId, _) cache =
    let futureOfFile = H.lookup fileId $ future cache
    in case futureOfFile of
        Just (prio, _) -> prio
        _              -> 0

updateFuture:: C.File -> BeladyCache -> BeladyCache
updateFuture f@(fileId, fileSize) cache =
    let (newPrio, cache') = currentRequestToPast f cache
    in cache' { futureCached = H.insert fileId newPrio fileSize $ futureCached cache }

currentRequestToPast :: C.File -> BeladyCache -> (FilePrio, BeladyCache)
currentRequestToPast (fileId, _) cache =
    let (prio, future') = H.alter updateRequest fileId $ future cache
    in (prio, cache { future = future' })

updateRequest :: Maybe (FilePrio, S.Seq OtherRequest) -> (FilePrio, Maybe (FilePrio, S.Seq OtherRequest))
updateRequest x@(Just (_, others))
    | S.null others = (0, Nothing) -- no more requests for this can, thus can be deleted
    | nextStatus == Changed = (0, Just(0, others))
    | otherwise = (prio, Just (prio, rest))
    where (nextStatus, maybePrio) :< rest = S.viewl others
          prio = fromMaybe 0 maybePrio

-- Initial preparation of the future requests
initFuture :: [FileRequest] -> FilePrio -> BeladyCache -> BeladyCache
initFuture [] _ cache         = cache
initFuture (request@(_, fileId, _) : rest) prio cache =
    let (_, future') = H.alter (alter' request prio) fileId $ future cache
        nextPrio = prio - 1
    in initFuture rest nextPrio cache { future = future' }

initFuture' :: [FileRequest] -> FilePrio -> BeladyCache -> BeladyCache
initFuture' requests prio cache = fst $ foldl' alter (cache, prio) requests
    where alter (c, p) r@(_, fileId, _) = (c { future = snd $ H.alter (alter' r p) fileId $ future cache }, p - 1)

alter' :: FileRequest -> FilePrio -> Maybe (FilePrio, S.Seq OtherRequest) -> (Empty, Maybe (FilePrio, S.Seq OtherRequest))
alter' (Read, _, _) prio Nothing = (Empty, Just (prio, S.empty))
alter' (Read, _, _) prio (Just (oldPrio, otherRequests)) = (Empty, Just (oldPrio, addToOtherRequests prio otherRequests))
alter' (_, _, _) _ Nothing = (Empty, Just (0, addNewFileVersion S.empty))
alter' (_, _, _) _ (Just (prio, otherRequests)) = (Empty, Just (prio, addNewFileVersion otherRequests))

addToOtherRequests :: FilePrio -> S.Seq OtherRequest -> S.Seq OtherRequest
addToOtherRequests prio otherRequests
    | S.null otherRequests = S.singleton (Same, Just prio)
    | isJust prioOfLast = otherRequests S.|> (Same, Just prio)
    | otherwise = rest S.|> (Changed, Just prio)
    where rest :> (_, prioOfLast) = S.viewr otherRequests

addNewFileVersion :: S.Seq OtherRequest -> S.Seq OtherRequest
addNewFileVersion requests = requests S.|> (Changed, Nothing)

switchToNewFileVersion :: Maybe (FilePrio, S.Seq OtherRequest) -> (Empty, Maybe (FilePrio, S.Seq OtherRequest))
switchToNewFileVersion Nothing = (Empty, Nothing)
switchToNewFileVersion v@(Just (_, others))
    | S.null others = (Empty, Nothing)
    | fileStatus == Changed && isJust maybePrio = (Empty, Just (prio, rest))
    | otherwise = (Empty, Just (0, rest))
    where (fileStatus, maybePrio) :< rest = S.viewl others
          prio = fromJust maybePrio
