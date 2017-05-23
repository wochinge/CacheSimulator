module File where

import Request (FileID, FileSize)

class File a where
    fileId :: a -> FileID
    fileSize :: a -> FileSize
