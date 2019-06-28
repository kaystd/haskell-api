{-# LANGUAGE DeriveGeneric #-}

module Db where

import GHC.Generics (Generic)
import Database.PostgreSQL.Simple

data DbConfig = DbConfig
  { dbName :: String
  , dbUser :: String
  , dbPassword :: String
  } deriving (Show, Generic)

newConn :: DbConfig -> IO Connection
newConn conf = connect defaultConnectInfo
                       { connectUser = dbUser conf
                       , connectPassword = dbPassword conf
                       , connectDatabase = dbName conf
                       }
