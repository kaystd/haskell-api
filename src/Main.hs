{-# LANGUAGE OverloadedStrings #-}

import Db
import Domain
import Views

import Web.Scotty
import Web.Scotty.Internal.Types (ActionT)
import Control.Monad.IO.Class
import Data.Pool
import Data.Aeson
import Database.PostgreSQL.Simple
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified Data.Text.Lazy as TL

makeDbConfig :: C.Config -> IO (Maybe Db.DbConfig)
makeDbConfig conf = do
  name <- C.lookup conf "database.name" :: IO (Maybe String)
  user <- C.lookup conf "database.user" :: IO (Maybe String)
  password <- C.lookup conf "database.password" :: IO (Maybe String)
  return $ DbConfig <$> name
                    <*> user
                    <*> password

-------------------------------------------------------------------------

main :: IO ()
main = do
  loadedConf <- C.load [C.Required "application.conf"]
  dbConf <- makeDbConfig loadedConf
  case dbConf of
    Nothing -> putStrLn "No database configuration found, terminating..."
    Just conf -> do
      pool <- createPool (newConn conf) close 1 60 10
      scotty 3000 $ do
        post "/register" $ do
          maybeRegUser <- getRegUserParam
          maybeUser <- addUser pool maybeRegUser
          viewUser maybeUser
        put "/change" $ do
          maybeUser <- getUserParam
          maybeResUser <- updateUser pool maybeUser
          viewUser maybeResUser
        get "/users" $ do
          users <- liftIO $ getUsersList pool
          usersList users
        get "/users/:user" $ do
          login <- param "user"
          maybeUser <- liftIO $ findUser pool login
          viewUser maybeUser
        delete "/users/:user" $ do
          login <- param "user"
          maybeUser <- deleteUser pool login
          viewUser maybeUser

-------------------------------------------------------------------------

getRegUserParam :: ActionT TL.Text IO (Maybe RegUser)
getRegUserParam = do
  b <- body
  return $ (decode b :: Maybe RegUser)

getUserParam :: ActionT TL.Text IO (Maybe User)
getUserParam = do
  b <- body
  return $ (decode b :: Maybe User)
