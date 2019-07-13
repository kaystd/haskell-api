{-# LANGUAGE OverloadedStrings #-}

module Domain where

import Data.Text.Lazy
import Data.Aeson

data RegUser = RegUser Text Text Text Text
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

data User = User Text Text Text
  deriving (Show)

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

data UserData = UserData Text Text
  deriving (Show)

instance FromJSON UserData where
  parseJSON (Object v) = UserData <$>
                         v .: "userData"  <*>
                         v .: "keyGost"

instance ToJSON UserData where
  toJSON (UserData userData keyGost) =
    object [ "userData" .= userData
           , "keyGost"  .= keyGost
           ]

data AccessToken = AccessToken Text
  deriving (Show)

instance FromJSON AccessToken where
  parseJSON (Object v) = AccessToken <$>
                         v .: "accessToken"

instance ToJSON AccessToken where
  toJSON (AccessToken accessToken) =
    object [ "accessToken" .= accessToken
           ]
