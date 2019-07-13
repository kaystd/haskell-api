{-# LANGUAGE OverloadedStrings #-}

import Auth
import Db
import Domain
import Views

import Web.Scotty
import Web.Scotty.Internal.Types (ActionT)
import Network.Wai
import Network.Wai.Middleware.HttpAuth
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Control.Monad.IO.Class
import Data.Pool
import Data.Aeson
import Control.Monad
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration
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

migrateDb :: Pool Connection -> IO ()
migrateDb pool = withResource pool $ \conn ->
  void $ withTransaction conn (runMigration (ctx conn))
    where ctx = MigrationContext cmd False
          cmd = MigrationCommands [ MigrationInitialization, MigrationDirectory "postgresql" ]

protectedResources :: Request -> IO Bool
protectedResources request = do
  let path = pathInfo request
  return $ protect path
    where protect (p : _) =  p == "login"
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
      migrateDb pool
      scotty 3000 $ do
        middleware logStdoutDev
        middleware $ basicAuth' (verifyCredentials pool)
                     "Haskell API Realm" { authIsProtected = protectedResources }

        post "/register" $ do
          maybeRegUser <- getRegUserParam
          maybeUser <- addUser pool maybeRegUser
          viewUser maybeUser

        get "/login" $ do
          headersList <- headers
          viewAccessToken . encodeJwt . extractBasicAuthLogin $ headersList

        get "/users" $ do
          headersList <- headers
          maybeLogin <- pure $ extractJwtLogin $ headersList
          case maybeLogin of
            Just "Admin" -> do
              users <- liftIO $ getUsersList pool
              usersList users
            _ -> viewError

        get "/user" $ do
          headersList <- headers
          maybeLogin <- pure $ extractJwtLogin $ headersList
          case maybeLogin of
            Just login -> do
              maybeUser <- liftIO $ findUser pool login
              viewUser maybeUser
            Nothing -> viewError

        put "/user" $ do
          maybeUser <- getUserParam
          headersList <- headers
          maybeLogin <- pure $ extractJwtLogin $ headersList
          case maybeLogin of
            Just login -> do
              maybeResUser <- updateUser pool login maybeUser
              viewUser maybeResUser
            Nothing -> viewError

        delete "/user" $ do
          headersList <- headers
          maybeLogin <- pure $ extractJwtLogin $ headersList
          case maybeLogin of
            Just login -> do
              maybeUser <- deleteUser pool login
              viewUser maybeUser
            Nothing -> viewError

-------------------------------------------------------------------------

getRegUserParam :: ActionT TL.Text IO (Maybe RegUser)
getRegUserParam = do
  b <- body
  return $ (decode b :: Maybe RegUser)

getUserParam :: ActionT TL.Text IO (Maybe UserData)
getUserParam = do
  b <- body
  return $ (decode b :: Maybe UserData)
