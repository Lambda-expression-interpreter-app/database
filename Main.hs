{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text.Lazy as Text (Text, pack)
import Data.Maybe
import Control.Applicative

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
        <$> v .: "user"
        <*> v .:? "pass"
        <*> v .:? "email"

newtype Response = Response {success :: Bool} deriving (Show)

instance ToJSON Response where
    toJSON :: Response -> Value
    toJSON (Response success) = object ["success" .= success]

-- Database information
connString :: ConnectInfo
connString = defaultConnectInfo {
    connectHost = "127.0.0.1",
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

addUser :: String -> String -> String -> IO Bool
addUser user pass email = do
    conn <- connect connString
    res <- query conn "SELECT add_user(?, ?, ?);" ([user, pass, email] :: [String]) :: IO [Only Bool]
    close conn
    return $ fromOnly $ head res

-- This function removes all the accounts with a given username
removeUser :: String -> String -> IO Bool
removeUser user email = do
    conn <- connect connString
    res <- query conn "SELECT delete_user(?, ?);" ([user, email] :: [String]) :: IO [Only Bool] -- need to check the database implementation
    close conn
    return $ fromOnly $ head res

main :: IO ()
main = do
    scotty 5000 $ do
        post "/login" $ do
            creds <- jsonData :: ActionM Credentials
            let username = user creds
                password = fromJust $ pass creds
            res <- liftIO $ checkIfUserExists username password
            json $ Response res
        post "/register" $ do
            creds <- jsonData :: ActionM Credentials
            let username = user creds
                password = fromJust $ pass creds
                emailAddr = fromJust $ email creds
            res <- liftIO $ addUser username password emailAddr
            json $ Response res
        post "/unregister" $ do
            creds <- jsonData :: ActionM Credentials
            let username = user creds
                emailAddr = fromJust $ email creds
            res <- liftIO $ removeUser username emailAddr
            json $ Response res