{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


module Main where

import Web.Scotty
import Data.Monoid ((<>))
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

data User = User {userId :: Int, userName :: String} deriving (Show, Generic)
instance ToJSON User
instance FromJSON User

sean :: User
sean = User {userId =1, userName = "Sean"}

jemille :: User
jemille = User {userId =2, userName = "Jemille"}

allUsers :: [User]
allUsers = [sean, jemille]

matchesId :: Int -> User -> Bool
matchesId id user = userId user == id

main :: IO ()

routes:: ScottyM ()
routes = do 
  get "/" $ do
    text "Main route"
  get "/hello" $ do
    text "Hello world"
  get "/hello/:name" $ do
    name <- param "name"
    text ("Hello " <> name <> "!")
  get "/users" $ do
    json allUsers
  get "/users/:id" $ do
    id <- param "id"
    json (filter (matchesId id) allUsers)
  post "/users" $ do
    user <- jsonData :: ActionM User
    json user

main = do
  putStrLn "Starting server at http://localhost:3000"
  scotty 3000 routes
  

  



