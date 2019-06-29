{-# LANGUAGE OverloadedStrings #-}

module Domain where

import Data.Text.Lazy
import Data.Aeson

data User = User Text Text Text Text
  deriving (Show)

instance FromJSON User where
  parseJSON (Object v) = User <$>
                         v .: "login"     <*>
                         v .: "password"  <*>
                         v .: "userData"      <*>
                         v .: "keyGost"

instance ToJSON User where
  toJSON (User login password userData keyGost) =
    object [ "login" .= login
           , "password" .= password
           , "userData" .= userData
           , "keyGost" .= keyGost
           ]
