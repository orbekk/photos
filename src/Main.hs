{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Aeson
import Data.List.Split (splitOn)
import GHC.Generics
import HFlags
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Servant
import System.Exit
import System.IO
import System.IO.Unsafe (unsafePerformIO)

import qualified Authentication
import Data
import PhotoStore

defineFlag "port" (8081 :: Int) "Port to serve on"
defineFlag "host" ("*6" :: String) "Host to serve on (*6 for ipv6 mode)"
defineFlag "pending_path" ("" :: String) "Path to pending albums"
defineFlag "photos_path" ("" :: String) "Path to permanent albums"
defineFlag "allowed_users" ("" :: String) "Comma separated list of emails"
defineFlag "client_ids" ("" :: String) "Comma separated list of client ids"
$(return [])  -- Somehow forces the flags to be set.

instance ToJSON Album
instance FromJSON Album

data RenameRequest = RenameRequest
  { from :: Album
  , to :: Album
  } deriving (Eq, Show, Generic)
instance FromJSON RenameRequest
instance ToJSON RenameError

type AuthenticationHeader = Header "X-Token" String
type PhotoApi =
       "albums"
           :> AuthenticationHeader
           :> Get '[JSON] [Album]
  :<|> "rename"
           :> AuthenticationHeader
           :> ReqBody '[JSON] RenameRequest
           :> Post '[JSON] (Either RenameError ())
  :<|> "quit" :> Get '[JSON] ()

type Token = String

{-# NOINLINE cache #-}
cache = unsafePerformIO (newMVar [])

isAuthenticated = Authentication.isAuthenticated clientIds users cache
    where clientIds = splitOn "," flags_client_ids
          users = splitOn "," flags_allowed_users

config = Config
  { pendingPath = flags_pending_path
  , photosPath = flags_photos_path
  }

checkAuthenticated :: Maybe Token -> EitherT ServantErr IO ()
checkAuthenticated (Just token) = do
  authenticated <- liftIO (isAuthenticated token)
  unless authenticated $ left err503 { errBody = "Not authenticated" }
checkAuthenticated Nothing = left err503 { errBody = "Missing token" }

server :: Server PhotoApi
server = albums
    :<|> rename
    :<|> quit
  where albums token = checkAuthenticated token >> liftIO (getAlbums config)

        rename token (RenameRequest from to) = do
          _ <- checkAuthenticated token
          liftIO $ runEitherT (renameAlbum config from to)

        quit = liftIO exitFailure

photoApi :: Proxy PhotoApi
photoApi = Proxy
app = logStdoutDev $ serve photoApi server

port = 8081
settings = setHost "*6" . setPort 8081 $ defaultSettings
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  $initHFlags "photos"
  print $ splitOn "," flags_allowed_users
  when (flags_pending_path == "") (die "--pending_path must be specified")
  when (flags_photos_path == "") (die "--photos_path must be specified")
  putStrLn $ "Starting server on port: " ++ (show port)
  runSettings settings app
