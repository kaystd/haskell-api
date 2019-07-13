module Views where

import Domain

import Web.Scotty
import Network.HTTP.Types.Status
import qualified Data.Text.Lazy as TL

usersList :: [User] -> ActionM ()
usersList users = json users

viewUser :: Maybe User -> ActionM ()
viewUser Nothing = status status400
viewUser (Just user) = json user

viewAccessToken :: Maybe TL.Text -> ActionM ()
viewAccessToken Nothing = status status400
viewAccessToken (Just accessToken) = json $ accessToken

viewError :: ActionM ()
viewError = status status403
