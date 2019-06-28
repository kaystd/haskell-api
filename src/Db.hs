{-# LANGUAGE DeriveGeneric #-}

module Db where

import GHC.Generics (Generic)
import Database.PostgreSQL.Simple
import Data.Pool

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

fetchSimple :: FromRow r => Pool Connection -> Query -> IO [r]
fetchSimple pool sql = withResource pool retrieve
  where retrieve conn = query_ conn sql

fetch :: (FromRow r, ToRow q) => Pool Connection -> q -> Query -> IO [r]
fetch pool args sql = withResource pool retrieve
  where retrieve conn = query conn sql args

execSql :: (ToRow q, FromRow r) => Pool Connection -> q -> Query -> IO [r]
execSql pool args sql = withResource pool ins
  where ins conn = query conn sql args

fetchSimpleT :: FromRow r => Pool Connection -> Query -> IO [r]
fetchSimpleT pool sql = withResource pool retrieve
  where retrieve conn = withTransaction conn $ query_ conn sql

fetchT :: (FromRow r, ToRow q) => Pool Connection -> q -> Query -> IO [r]
fetchT pool args sql = withResource pool retrieve
  where retrieve conn = withTransaction conn $ query conn sql args

execSqlT :: (ToRow q, FromRow r) => Pool Connection -> q -> Query -> IO [r]
execSqlT pool args sql = withResource pool ins
  where ins conn = withTransaction conn $ query conn sql args
