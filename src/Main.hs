{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

import Data.Monoid (mconcat)

main :: IO ()
main = do
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
