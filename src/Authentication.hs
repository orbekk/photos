{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
module Authentication (User, queryUser, isAuthenticated) where

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

isAuthenticated :: [String] -> MVar [Token] -> Token -> IO Bool
isAuthenticated allowedUsers tokenCache token = runEitherT runner >>= return . fromEither
    where runner :: EitherT Bool IO Bool
          runner = do
            ts <- lift $ readMVar tokenCache
            _ <- leftIf (token `elem` ts) True
            user <- lift $ queryUser token
            email' <- return $ fromMaybe "" (user >>= return . email)
            _ <- leftIf (not (email' `elem` allowedUsers)) False
            tokens <- lift $ takeMVar tokenCache
            lift $ putMVar tokenCache (token:tokens)
            return True

          leftIf True x = left x
          leftIf False x = right x
