{-# LANGUAGE OverloadedStrings #-}

module Request
    ( getFileRequests
    , forEachFileRequestIn
    , FileRequest
    , FileID
    , FileSize
    , RequestType(..)
    ) where

import qualified Data.ByteString       as B (ByteString, hGetContents)
import qualified Data.ByteString.Char8 as B (lines, readInt, split)
import           System.IO

data RequestType = Read | Write | Remove
type FileRequest = (RequestType, B.ByteString, FileSize)
type FileID = B.ByteString
type FileSize = Int

forEachFileRequestIn :: ([FileRequest] -> a) -> String -> IO a
forEachFileRequestIn function fileName =
    withFile fileName ReadMode (\handle -> do
        operations <- getFileRequests handle
        return $! function operations
    )

getFileRequests :: Handle -> IO [FileRequest]
getFileRequests handle = do
    file <- B.hGetContents handle
    let fileOperationLines = B.lines file
    return $ map convert fileOperationLines

convert :: B.ByteString -> FileRequest
convert line =
    let [_, _, operation, fileID, fileSize] = B.split ' ' line
        accessType = textToAccessType operation
        size = toInt fileSize
    in (accessType, fileID, size)

textToAccessType :: B.ByteString -> RequestType
textToAccessType "write" = Write
textToAccessType "read"  = Read
textToAccessType _       = Remove

toInt :: B.ByteString -> FileSize
toInt asByteString =
    let Just(asInt, _) = B.readInt asByteString
    in asInt
