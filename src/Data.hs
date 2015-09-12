{-# LANGUAGE DeriveGeneric #-}
module Data where

import GHC.Generics

data Album = Album
  { name :: String
  , pending :: Bool
  } deriving (Eq, Show, Generic)
