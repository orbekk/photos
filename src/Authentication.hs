{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
module Authentication (User, queryUser) where

import Data.Aeson
import Data.Maybe
import Data.Either.Extra
import GHC.Generics
import Network.HTTP.Conduit
import Data.ByteString.Lazy.Char8 (unpack)
import Control.Exception
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Control.Concurrent.MVar

makeUrl = ("https://www.googleapis.com/oauth2/v3/tokeninfo?id_token="++)
type Token = String

data User = User
  { email :: String
  , name :: String
  } deriving (Eq, Show, Generic)

instance FromJSON User

queryUser :: Token -> IO (Maybe User)
queryUser token = (queryUser' token) `catch` \e -> do
  print (e :: HttpException)
  return Nothing

queryUser' token = do
  response <- simpleHttp (makeUrl token)
  return (decode response)

isAuthenticated :: [[String]] -> MVar [Token] -> Token -> IO Bool
isAuthenticated allowedUsers tokenCache token = runEitherT f >>= return . fromEither
    where f :: EitherT Bool IO Bool
          f = do
            ts <- lift $ readMVar tokenCache
            _ <- test (token `elem` ts)
            user <- lift $ queryUser token
            return False

          test False = return ()
          test True = left True

