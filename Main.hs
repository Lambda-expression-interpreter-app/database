{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text.Lazy as Text (Text, pack)
import Data.Maybe
import Control.Applicative
import Control.Exception

import Database.MongoDB hiding (Value, addUser, removeUser)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Web.Scotty

data Credentials = Credentials {
    user :: String,
    pass :: Maybe String,
    email :: Maybe String
} deriving (Show)

instance FromJSON Credentials where
    parseJSON :: Value -> Parser Credentials
    parseJSON = withObject "Credentials" $ \v -> Credentials
        <$> v .: "username"
        <*> v .:? "password"
        <*> v .:? "email"

data Response = BoolResp Bool | StringResp String deriving (Show)

instance ToJSON Response where
    toJSON :: Response -> Value
    toJSON (BoolResp success) = object ["success" .= success]
    toJSON (StringResp msg) = object ["success" .= msg]

checkIfUserExists :: String -> String -> IO Bool
checkIfUserExists username password = do
    pipe <- connect (host "mongodb")
    docs <- access pipe master "credentials" $ do
        cursor <- find (select ["username" =: username, "password" =: password] "users")
        rest cursor
    close pipe
    return $ not $ Prelude.null docs

addUser :: String -> String -> String -> IO String
addUser user pass email = do
    pipe <- connect (host "mongodb")
    exists <- checkIfUserExists user pass
    if exists
        then return "User already exists."
        else do
            access pipe master "credentials" $ insert "users" ["username" =: user, "password" =: pass, "email" =: email]
            close pipe
            return "Success"

removeUser :: String -> String -> IO String
removeUser user email = do
    pipe <- connect (host "mongodb")
    result <- access pipe master "credentials" $ do
        findAndModify (select ["username" =: user, "email" =: email] "users") ["$unset" =: ["username" =: ("" :: String), "email" =: ("" :: String)]]
    close pipe
    return $ case result of
        Left _ -> "User doesn't exist."
        _ -> "Success"

main :: IO ()
main = do
    scotty 6000 $ do
        post "/login" $ do
            creds <- jsonData :: ActionM Credentials
            let username = user creds
                password = fromJust $ pass creds
            res <- liftIO $ checkIfUserExists username password
            json $ BoolResp res
        post "/register" $ do
            creds <- jsonData :: ActionM Credentials
            let username = user creds
                password = fromJust $ pass creds
                emailAddr = fromJust $ email creds
            res <- liftIO $ addUser username password emailAddr
            json $ StringResp res
        post "/unregister" $ do
            creds <- jsonData :: ActionM Credentials
            let username = user creds
                emailAddr = fromJust $ email creds
            res <- liftIO $ removeUser username emailAddr
            json $ StringResp res