{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
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
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Control.Concurrent.MVar

makeUrl = ("https://www.googleapis.com/oauth2/v3/tokeninfo?id_token="++)
type Token = String

data User = User
  { email :: String
  , name :: String
  , aud :: String
  } deriving (Eq, Show, Generic)
instance FromJSON User

queryUser :: Token -> IO (Maybe User)
queryUser token = runEitherT (lift queryUser') >>= \case
  Left e -> print (e :: HttpException) >> return Nothing
  Right response -> return response
  where queryUser' = decode <$> simpleHttp (makeUrl token)

isAuthenticated :: [String] -> [String] -> MVar [Token] -> Token -> IO Bool
isAuthenticated clientIds allowedUsers tokenCache token = do
  ts <- readMVar tokenCache
  if token `elem` ts
  then return True
  else isJust <$> isValidUser
      where isValidUser = runMaybeT $ do
                            Just user <- lift $ queryUser token
                            True <- return $ email user `elem` allowedUsers
                            True <- return $ aud user `elem` clientIds
                            tokens <- lift $ takeMVar tokenCache
                            lift $ putMVar tokenCache (token:tokens)
                            return ()
