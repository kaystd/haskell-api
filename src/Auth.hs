{-# LANGUAGE OverloadedStrings #-}

module Auth where

import Db

import Network.Wai
import Network.HTTP.Types.URI
import Control.Monad.IO.Class
import Data.Pool
import Data.Hash.MD5
import Database.PostgreSQL.Simple
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import qualified Data.Text.Lazy as TL
import qualified Data.Text as TI

verifyCredentials :: Pool Connection -> Request -> B.ByteString -> B.ByteString -> IO Bool
verifyCredentials pool request login password = do
  pwd <- findUserPassByLogin pool (BC.unpack login)
  return $ (compareUsers login request) && (comparePasswords pwd (BC.unpack password))
    where comparePasswords Nothing _          = False
          comparePasswords (Just p) password  = p == (md5s $ Str password)
          compareUsers login request =  (TI.unpack . last . pathInfo $ request) == (BC.unpack login)

verifyAdminCredentials :: Pool Connection -> B.ByteString -> B.ByteString -> IO Bool
verifyAdminCredentials pool login password = do
  pwd <- findUserPassByLogin pool (BC.unpack login)
  return $ (compareUsers login) && (comparePasswords pwd (BC.unpack password))
    where comparePasswords Nothing _          = False
          comparePasswords (Just p) password  = p == (md5s $ Str password)
          compareUsers login =  (BC.unpack login) == "Admin"
