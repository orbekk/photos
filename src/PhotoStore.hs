{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module PhotoStore where

import Control.Exception.Base
import Data
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import GHC.Generics
import System.Directory
import System.FilePath
import Prelude
import Control.Monad
import Control.Exception
import System.IO.Error
import Data.List

data Config = Config
  { pendingPath :: String
  , photosPath :: String
  }

getDirectoryFiles path = getDirectoryContents path >>= return . filter f
  where f filename = not (filename `elem` [".", ".."])

validAlbumName name =
     name /= "."
  && name /= ".."

getAlbums :: Config -> IO [Album]
getAlbums config = do
  pending <- getDirectoryFiles (pendingPath config)
  permanent <- getDirectoryFiles (photosPath config)
  return ([Album name True | name <- pending] ++
          [Album name False | name <- permanent])

albumDirectory :: Config -> Album -> FilePath
albumDirectory config album
  | pending album = joinPath [pendingPath config, name album]
  | otherwise     = joinPath [photosPath config, name album]

data RenameError = SameSourceAndTarget
                 | DuplicateFilesExist
    deriving (Eq, Show, Generic)

renameAlbum :: Config -> Album -> Album -> EitherT RenameError IO ()
renameAlbum config source target = do
    if sourceDir == targetDir then left SameSourceAndTarget
                              else return ()
    srcFiles <- lift $ getDirectoryFiles sourceDir
    lift $ createDirectoryIfMissing False targetDir
    existingFiles <- lift $ getDirectoryFiles targetDir
    if not . null $ intersect srcFiles existingFiles
        then left DuplicateFilesExist
        else return ()
    let rename filename = renameFile (joinPath [sourceDir, filename])
                                     (joinPath [targetDir, filename])
    lift $ mapM rename srcFiles
    lift $ removeDirectory sourceDir
    return ()
  where sourceDir = albumDirectory config source
        targetDir = albumDirectory config target
