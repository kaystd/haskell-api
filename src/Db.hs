{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Db where
import Domain

import Web.Scotty.Internal.Types (ActionT)
import GHC.Generics (Generic)
import Control.Monad.IO.Class
import Database.PostgreSQL.Simple
import Data.Pool
import Data.Hash.MD5
import qualified Data.Text.Lazy as TL

data DbConfig = DbConfig
  { dbName :: String
  , dbUser :: String
  , dbPassword :: String
  } deriving (Show, Generic)

-------------------------------------------------------------------------

newConn :: DbConfig -> IO Connection
newConn conf = connect defaultConnectInfo
                       { connectUser = dbUser conf
                       , connectPassword = dbPassword conf
                       , connectDatabase = dbName conf
                       }

-------------------------------------------------------------------------

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

-------------------------------------------------------------------------

findUserPassByLogin :: Pool Connection -> String -> IO (Maybe String)
findUserPassByLogin pool login = do
  res <- liftIO $ fetch pool (Only login) "SELECT password FROM user WHERE login = ?"
  return $ password res
    where password [(pwd)] = Just pwd
          password _ = Nothing

-------------------------------------------------------------------------

userList :: Pool Connection -> IO [User]
userList pool = do
  res <- fetchSimple pool
                     "SELECT login, userData, keyGost FROM user ORDER BY login ASC" {-:: IO [(TL.Text, TL.Text, TL.Text)]-}
  return $ map (\(login, userData, keyGost) -> User login userData keyGost) res

findUser :: Pool Connection -> TL.Text -> IO (Maybe User)
findUser pool login = do
  res <- fetch pool (Only login) "SELECT login, userData, keyGost FROM user WHERE login = ?"
  return $ oneUser res

addUser :: Pool Connection -> Maybe RegUser -> ActionT TL.Text IO (Maybe User)
addUser pool Nothing = return Nothing
addUser pool (Just (RegUser login password userData keyGost)) = do
  res <- liftIO $ execSqlT pool [login, (TL.pack $ md5s $ Str $ TL.unpack password), userData, keyGost]
                                "INSERT INTO user (login, password, userData, keyGost) VALUES (?, ?, ?, ?) RETURNING login userData keyGost"
  return $ oneUser res

updateUser :: Pool Connection -> Maybe User -> ActionT TL.Text IO (Maybe User)
updateUser pool Nothing = return Nothing
updateUser pool (Just (User login userData keyGost)) = do
  res <- liftIO $ execSqlT pool [userData, keyGost, login]
                                "UPDATE user SET userData = ?, keyGost = ? WHERE login = ? RETURNING login userData keyGost"
  return $ oneUser res

deleteUser :: Pool Connection -> TL.Text -> ActionT TL.Text IO (Maybe User)
deleteUser pool login = do
  res <- liftIO $ execSqlT pool [login]
                                "DELETE FROM user WHERE login = ?"
  return $ oneUser res

-------------------------------------------------------------------------

oneUser :: [(TL.Text, TL.Text, TL.Text)] -> Maybe User
oneUser ((login, userData, keyGost) : _) = Just $ User login userData keyGost
oneUser _ = Nothing
