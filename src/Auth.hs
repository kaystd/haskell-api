module Auth where

import Db

import Network.Wai
import Network.HTTP.Types.URI
import Control.Monad.IO.Class
import Data.Pool
import Data.Hash.MD5
import Database.PostgreSQL.Simple
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString.Base64.Lazy as BBL
import qualified Web.JWT as JWT
import qualified Data.Map as Map

verifyCredentials :: Pool Connection -> Request -> B.ByteString -> B.ByteString -> IO Bool
verifyCredentials pool request login password = do
  pwd <- findUserPassByLogin pool (BC.unpack login)
  return $ comparePasswords pwd (BC.unpack password)
    where comparePasswords Nothing _          = False
          comparePasswords (Just p) password  = p == (md5s $ Str password)

extractBasicAuthLogin :: [(TL.Text, TL.Text)] -> Maybe TL.Text
extractBasicAuthLogin headersList = case authTuple of
  [(_, authData)] -> Just $ TLE.decodeUtf8 . BLC.takeWhile (/= ':') $ BBL.decodeLenient . TLE.encodeUtf8 . TL.strip $ TL.dropWhile (/= ' ') authData
  _ -> Nothing
  where authTuple = filter (\v -> (TL.unpack $ fst v) == "Authorization") headersList

extractJwtLogin :: TL.Text -> [(TL.Text, TL.Text)] -> Maybe TL.Text
extractJwtLogin secret headersList = case authTuple of
  [(_, authData)] -> extractLogin . decodeJwt secret $ TL.strip $ TL.dropWhile (/= ' ') authData
  _ -> Nothing
  where authTuple = filter (\v -> (TL.unpack $ fst v) == "Authorization") headersList
        extractLogin (Just jwt) = Just . TL.fromStrict . getValue $ (JWT.unClaimsMap $ JWT.unregisteredClaims jwt) Map.! "login"
        extractLogin Nothing = Nothing
        getValue (String value) = value

encodeJwt :: TL.Text -> Maybe TL.Text -> Maybe TL.Text
encodeJwt _ Nothing = Nothing
encodeJwt secret (Just login) = Just . TL.fromStrict $ JWT.encodeSigned key mempty cs
  where cs = mempty { JWT.iss = JWT.stringOrURI $ TL.toStrict login
                    , JWT.unregisteredClaims = JWT.ClaimsMap $ Map.fromList [("login", (String $ TL.toStrict login))]
                    }
        key = JWT.hmacSecret $ TL.toStrict secret

decodeJwt :: TL.Text -> TL.Text -> Maybe JWT.JWTClaimsSet
decodeJwt secret input = getClaims mJwt
  where mJwt = JWT.decodeAndVerifySignature (JWT.hmacSecret $ TL.toStrict secret) $ TL.toStrict input
        getClaims (Just jwt) = Just $ JWT.claims jwt
        getClaims Nothing = Nothing
