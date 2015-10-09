{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Aeson
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import HFlags
import System.Exit
import Control.Monad
import Data
import PhotoStore
import Control.Monad.IO.Class
import Control.Monad.Trans.Either

defineFlag "port" (8081 :: Int) "Port to serve on"
defineFlag "host" ("*6" :: String) "Host to serve on (*6 for ipv6 mode)"
defineFlag "pending_path" ("" :: String) "Path to pending albums"
defineFlag "photos_path" ("" :: String) "Path to permanent albums"
$(return [])  -- Somehow forces the flags to be set.

instance ToJSON Album
instance FromJSON Album

data RenameRequest = RenameRequest
  { from :: Album
  , to :: Album
  } deriving (Eq, Show, Generic)
instance FromJSON RenameRequest
instance ToJSON RenameError

type PhotoApi =
       "albums" :> Get '[JSON] [Album]
  :<|> "rename" :> ReqBody '[JSON] RenameRequest :> Post '[JSON] (Either RenameError ())
  :<|> "test"   :> Header "X-Token" String :> Get '[JSON] ()
-- Introduce request header containing auth information.

config = Config 
  { pendingPath = flags_pending_path
  , photosPath = flags_photos_path
  }

server :: Server PhotoApi
server = albums
    :<|> rename
    :<|> test

  where albums = liftIO (getAlbums config)

        rename :: RenameRequest -> EitherT ServantErr IO (Either RenameError ())
        rename (RenameRequest from to) = liftIO $
          runEitherT (renameAlbum config from to)

        test token = return ()

photoApi :: Proxy PhotoApi
photoApi = Proxy

app :: Application
app = serve photoApi server

port = 8081
settings :: Settings
settings = setHost "*6" . setPort 8081 $ defaultSettings

main :: IO ()
main = do
  $initHFlags "photos"
  when (flags_pending_path == "") (die "--pending_path must be specified")
  when (flags_photos_path == "") (die "--photos_path must be specified")
  putStrLn $ "Starting server on port: " ++ (show port)
  runSettings settings app
