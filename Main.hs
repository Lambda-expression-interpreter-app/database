{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.MySQL.Simple

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
    res <- query conn "CALL add_user(?, ?, ?);" ([user, pass, email] :: [String]) :: IO [Only Int]
    close conn
    return $ null res

main :: IO ()
main = do
    -- res <- query conn "SELECT (2 + 2) FROM DUAL;" ([] :: [Int]) :: IO [Only Int]
    res <- checkIfUserExists "John" "Hello!"
    print res