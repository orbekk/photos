module PhotoStore where

import Data
import System.Directory

data Config = Config
  { pendingPath :: String
  , photosPath :: String
  }

validAlbumName name =
     name /= "."
  && name /= ".."

getAlbums :: Config -> IO [Album]
getAlbums config = do
  pending <- getDirectoryContents (pendingPath config)
  permanent <- getDirectoryContents (photosPath config)
  return ([Album name True | name <- filter validAlbumName pending] ++
          [Album name False | name <- filter validAlbumName permanent])
