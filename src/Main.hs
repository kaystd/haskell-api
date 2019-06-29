{-# LANGUAGE OverloadedStrings #-}

import Db
import Views

import Web.Scotty
import Data.Monoid (mconcat)
import Data.Pool
import Database.PostgreSQL.Simple
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C

makeDbConfig :: C.Config -> IO (Maybe Db.DbConfig)
makeDbConfig conf = do
  name <- C.lookup conf "database.name" :: IO (Maybe String)
  user <- C.lookup conf "database.user" :: IO (Maybe String)
  password <- C.lookup conf "database.password" :: IO (Maybe String)
  return $ DbConfig <$> name
                    <*> user
                    <*> password

main :: IO ()
main = do
  loadedConf <- C.load [C.Required "application.conf"]
  dbConf <- makeDbConfig loadedConf
  case dbConf of
    Nothing -> putStrLn "No database configuration found, terminating..."
    Just conf -> do
      pool <- createPool (newConn conf) close 1 60 10
      scotty 3000 $ do
        get "/register" $ do
          html $ "<h1>Register</h1>"
        get "/login" $ do
          html $ "<h1>Login</h1>"
        get "/users" $ do
          html $ "<h1>Users</h1>"
        get "/users/:user" $ do
          user <- param "user"
          html $ mconcat ["<h1>User ", user, "</h1>"]
