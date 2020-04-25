import Auth
import Db
import Domain
import Views

import Web.Scotty
import Web.Scotty.Internal.Types (ActionT)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.HttpAuth
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Control.Monad.IO.Class
import Data.Pool
import Data.Aeson
import Data.Word
import GHC.Generics (Generic)
import Control.Monad
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration
import Data.Streaming.Network.Internal
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified Data.Text.Lazy as TL

data AppConfig = AppConfig
  { dbConfig :: DbConfig
  , appHost :: String
  , appPort :: Int
  , secret :: String
  } deriving (Show, Generic)

-------------------------------------------------------------------------

makeDbConfig :: C.Config -> IO (Maybe Db.DbConfig)
makeDbConfig conf = do
  host <- C.lookup conf "database.host"
  port <- C.lookup conf "database.port"
  name <- C.lookup conf "database.name"
  user <- C.lookup conf "database.user"
  password <- C.lookup conf "database.password"
  return $ DbConfig <$> host <*> port <*> name <*> user <*> password

makeAppConfig :: C.Config -> IO (Maybe AppConfig)
makeAppConfig conf = do
  db <- makeDbConfig conf
  host <- C.lookup conf "app.host"
  port <- C.lookup conf "app.port"
  secret <- C.lookup conf "app.secret"
  return $ AppConfig <$> db <*> host <*> port <*> secret

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
  appConf <- makeAppConfig loadedConf
  case appConf of
    Nothing -> putStrLn "No configuration found, terminating..."
    Just conf -> do
      pool <- createPool (newConn $ dbConfig conf) close 1 60 10
      migrateDb pool
      scottyOpts (Options 1 (setPort (appPort conf) $ setHost (Host $ appHost conf) defaultSettings)) $ do
        middleware $ cors ( const $ Just (simpleCorsResourcePolicy {corsRequestHeaders = ["Authorization", "Content-Type"] }))
        middleware logStdoutDev
        middleware $ basicAuth' (verifyCredentials pool)
                     "Haskell API Realm" { authIsProtected = protectedResources }

        post "/register" $ do
          maybeRegUser <- getRegUserParam
          maybeUser <- addUser pool maybeRegUser
          viewUser maybeUser

        get "/login" $ do
          headersList <- headers
          viewAccessToken $ encodeJwt (TL.pack $ secret conf) $ extractBasicAuthLogin headersList

        get "/users" $ do
          headersList <- headers
          maybeLogin <- pure $ extractJwtLogin (TL.pack $ secret conf) headersList
          case maybeLogin of
            Just "Admin" -> do
              users <- liftIO $ getUsersList pool
              usersList users
            _ -> viewError

        get "/user" $ do
          headersList <- headers
          maybeLogin <- pure $ extractJwtLogin (TL.pack $ secret conf) headersList
          case maybeLogin of
            Just login -> do
              maybeUser <- liftIO $ findUser pool login
              viewUser maybeUser
            Nothing -> viewError

        put "/user" $ do
          maybeUser <- getUserParam
          headersList <- headers
          maybeLogin <- pure $ extractJwtLogin (TL.pack $ secret conf) headersList
          case maybeLogin of
            Just login -> do
              maybeResUser <- updateUser pool login maybeUser
              viewUser maybeResUser
            Nothing -> viewError

        delete "/user" $ do
          headersList <- headers
          maybeLogin <- pure $ extractJwtLogin (TL.pack $ secret conf) headersList
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
