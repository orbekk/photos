{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Aeson
import GHC.Generics
import HFlags
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Servant
import System.Exit
import System.IO.Unsafe (unsafePerformIO)
import Data.List.Split

import qualified Authentication
import Data
import PhotoStore

defineFlag "port" (8081 :: Int) "Port to serve on"
defineFlag "host" ("*6" :: String) "Host to serve on (*6 for ipv6 mode)"
defineFlag "pending_path" ("" :: String) "Path to pending albums"
defineFlag "photos_path" ("" :: String) "Path to permanent albums"
defineFlag "allowed_users" ("" :: String) "Comma separated list of emails"
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
  :<|> "test"   :> Header "X-Token" String :> Get '[JSON] String
-- Introduce request header containing auth information.

type Token = String

isAuthenticated = Authentication.isAuthenticated users cache
    where users = splitOn "," flags_allowed_users
          cache = unsafePerformIO (newMVar [])

config = Config
  { pendingPath = flags_pending_path
  , photosPath = flags_photos_path
  }

whenAuthenticated :: Maybe Token -> EitherT ServantErr IO a -> EitherT ServantErr IO a
whenAuthenticated (Just token) action = liftIO (isAuthenticated token) >>= \case
  True -> action
  False -> left err503 { errBody = "Not authenticated" }
whenAuthenticated Nothing _ = left err503 { errBody = "Missing token" }

server :: Server PhotoApi
server = albums
    :<|> rename
    :<|> test
  where albums = liftIO (getAlbums config)

        rename :: RenameRequest -> EitherT ServantErr IO (Either RenameError ())
        rename (RenameRequest from to) = liftIO $
          runEitherT (renameAlbum config from to)

        test = (`whenAuthenticated` test')
        test' = return "Yay"

photoApi :: Proxy PhotoApi
photoApi = Proxy

app :: Application
app = logStdoutDev $ serve photoApi server

port = 8081
settings :: Settings
settings = setHost "*6" . setPort 8081 $ defaultSettings

main :: IO ()
main = do
  $initHFlags "photos"
  print $ splitOn "," flags_allowed_users
  when (flags_pending_path == "") (die "--pending_path must be specified")
  when (flags_photos_path == "") (die "--photos_path must be specified")
  putStrLn $ "Starting server on port: " ++ (show port)
  runSettings settings app
