module Util.CacheSize
    ( maxCacheSize
    ) where

import           Data.Set as S hiding (size)
import           Request

type Storage = (Set FileID, FileSize)

maxCacheSize :: String -> IO Int
maxCacheSize fileName = (`getCacheSize` (S.empty, 0)) `forEachFileRequestIn` fileName

getCacheSize :: [FileRequest] -> Storage -> FileSize
getCacheSize [] (_, size) = size

getCacheSize ((Read, fileID, size) : rest) c@(files, currentSize)
    | fileID `S.member` files = max currentSize $ getCacheSize rest c
    | otherwise = max currentSize $ getCacheSize rest (S.insert fileID files, newSize)
    where newSize = currentSize + size

getCacheSize ((Write, fileID, size) : rest) (files, currentSize) =
        max currentSize $ getCacheSize rest (S.insert fileID files, newSize)
        where newSize = currentSize + size

getCacheSize ((Remove, fileID, size) : rest) c@(files, currentSize)
    | fileID `S.member` files = max currentSize $ getCacheSize rest (delete fileID files, currentSize - size)
    | otherwise = max (currentSize + size) $ getCacheSize rest c
