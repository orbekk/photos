{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module PhotoStore where

import Control.Exception
import Control.Exception.Base
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Data
import Data.List
import GHC.Generics
import Prelude
import System.Directory
import System.FilePath
import System.IO.Error

data Config = Config
  { pendingPath :: String
  , photosPath :: String
  }

getDirectoryFiles path = getDirectoryContents path >>= return . filter f
  where f filename = not (filename `elem` [".", ".."])

validAlbumName name = not ("." `isPrefixOf` name)

getAlbums :: Config -> IO [Album]
getAlbums config = do
  pending <- getDirectoryFiles (pendingPath config)
  permanent <- getDirectoryFiles (photosPath config)
  return ([Album name True | name <- sort pending] ++
          [Album name False | name <- sort permanent])

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
