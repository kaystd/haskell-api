{-# LANGUAGE OverloadedStrings #-}

module Domain where

import Data.Text.Lazy
import Data.Aeson

data RegUser = RegUser Text Text Text Text
  deriving (Show)

data User = User Text Text Text
  deriving (Show)

instance FromJSON RegUser where
  parseJSON (Object v) = RegUser <$>
                         v .: "login"     <*>
                         v .: "password"  <*>
                         v .: "userData"  <*>
                         v .: "keyGost"

instance ToJSON RegUser where
  toJSON (RegUser login password userData keyGost) =
    object [ "login"    .= login
           , "password" .= password
           , "userData" .= userData
           , "keyGost"  .= keyGost
           ]

instance FromJSON User where
  parseJSON (Object v) = User <$>
                         v .: "login"     <*>
                         v .: "userData"  <*>
                         v .: "keyGost"

instance ToJSON User where
  toJSON (User login userData keyGost) =
    object [ "login"    .= login
           , "userData" .= userData
           , "keyGost"  .= keyGost
           ]
