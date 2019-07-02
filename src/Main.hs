{-# LANGUAGE OverloadedStrings #-}

import Auth
import Db
import Domain
import Views

import Web.Scotty
import Web.Scotty.Internal.Types (ActionT)
import Network.Wai
import Network.Wai.Middleware.HttpAuth
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

protectedResources :: Request -> IO Bool
protectedResources request = do
  let path = pathInfo request
  return $ protect path
    where protect (p : _) =  p == "users"
          protect _       =  False

protectedAdminResources :: Request -> IO Bool
protectedAdminResources request = do
  let path = pathInfo request
  return $ protect path
    where protect (p : _) =  p == "admin"
          protect _       =  False

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
        middleware $ basicAuth (verifyAdminCredentials pool)
                     "Haskell API Realm" { authIsProtected = protectedAdminResources }
        middleware $ basicAuth' (verifyCredentials pool)
                     "Haskell API Realm" { authIsProtected = protectedResources }

        post "/register" $ do
          maybeRegUser <- getRegUserParam
          maybeUser <- addUser pool maybeRegUser
          viewUser maybeUser

        get "/admin/users" $ do
          users <- liftIO $ getUsersList pool
          usersList users

        get "/users/:user" $ do
          login <- param "user"
          maybeUser <- liftIO $ findUser pool login
          viewUser maybeUser

        put "/users/:user" $ do
          login <- param "user"
          maybeUser <- getUserParam
          maybeResUser <- updateUser pool login maybeUser
          viewUser maybeResUser

        delete "/users/:user" $ do
          login <- param "user"
          maybeUser <- deleteUser pool login
          viewUser maybeUser

-------------------------------------------------------------------------

getRegUserParam :: ActionT TL.Text IO (Maybe RegUser)
getRegUserParam = do
  b <- body
  return $ (decode b :: Maybe RegUser)

getUserParam :: ActionT TL.Text IO (Maybe UserData)
getUserParam = do
  b <- body
  return $ (decode b :: Maybe UserData)
