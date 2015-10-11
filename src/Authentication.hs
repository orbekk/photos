{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
module Authentication (User, queryUser) where

import Data.Aeson
import GHC.Generics
import Network.HTTP.Conduit
import Data.ByteString.Lazy.Char8 (unpack)
import Control.Exception

makeUrl = ("https://www.googleapis.com/oauth2/v3/tokeninfo?id_token="++)

data User = User
  { email :: String
  , name :: String
  } deriving (Eq, Show, Generic)

instance FromJSON User

queryUser :: String -> IO (Maybe User)
queryUser token = (queryUser' token) `catch` \e -> do
  print (e :: HttpException)
  return Nothing

queryUser' token = do
  response <- simpleHttp (makeUrl token)
  return (decode response)
