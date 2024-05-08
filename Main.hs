{-# LANGUAGE OverloadedStrings #-}

module Main where

import Debug.Trace
import Data.Text.Lazy as Text (Text, pack)
import Data.Maybe
import Control.Applicative
import Control.Exception

import Database.MySQL.Simple
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

-- Database information
connString :: ConnectInfo
connString = defaultConnectInfo {
    connectHost = "database",
    connectPort = 3306,
    connectUser = "root",
    connectPassword = "idp_database",
    connectDatabase = "credentials"
}

checkIfUserExists :: String -> String -> IO Bool
checkIfUserExists user pass = do
    conn <- connect connString
    res <- query conn "SELECT check_user_password(?, ?);" ([user, pass] :: [String]) :: IO [Only Bool]
    close conn
    return $ fromOnly $ head res

addUser :: String -> String -> String -> IO String
addUser user pass email = do
    conn <- connect connString
    res <- try $ execute conn "CALL add_user(?, ?, ?);" ([user, pass, email] :: [String])
    close conn
    case res of
        Left (SomeException _) -> return "Failed to register user."
        Right x -> return $ if x > 0 then "Success" else "User already exists."

removeUser :: String -> String -> IO String
removeUser user email = do
    conn <- connect connString
    res <- try $ execute conn "CALL delete_user(?, ?);" ([user, email] :: [String])
    close conn
    case res of
        Left (SomeException _) -> return "Failed to unregister user."
        Right x -> return $ if x > 0 then "Success" else "User does not exist."

main :: IO ()
main = do
    scotty 25000 $ do
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