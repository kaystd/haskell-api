module Views where

import Domain

import Web.Scotty
import qualified Data.Text.Lazy as TL

usersList :: [User] -> ActionM ()
usersList users = json users

viewUser :: Maybe User -> ActionM ()
viewUser Nothing = json ()
viewUser (Just user) = json user
