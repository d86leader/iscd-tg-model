{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Actions
import Control.Monad          (forM_)
import Control.Monad.Fail     (fail)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid            ((<>))
import Data.Text              (Text, unpack, unwords, words)
import Database.Persist       (count, (==.), Key)
import Database.Persist.Sql   (transactionSave)
import Requests
import System.IO              (hFlush, stdout)
import Text.Read              (readMaybe)

import Database.Control
    (SqlM, doMigrations, makeConnectionPool, runDatabase)
import Database.Types
    ( EntityField (UserName, UserPassword), User
    , Rights (Read, Write, Take, Grant)
    )

import qualified Data.Text.IO as TIO

import Prelude hiding (fail, read, take, unwords, words)

checkShould :: SqlM (Either Text a) -> SqlM a
checkShould v = v >>= \case
    Left message -> (liftIO . TIO.putStrLn) ("Model error: " <> message)
                    >> fail "Aborting due to model error"  -- Maybe it's bad idea to throw error
    Right val    -> pure val


parseStr :: Key User -> Text -> SqlM Text
parseStr current_user str =
    case words str of
        "create" : "user" : user : password : _ ->
            createUserSafe current_user user password

        "create" : "object" : name : rest -> do
            let text = unwords rest
            createObjectSafe current_user name text

        "write" : object : rest -> do
            let text = unwords rest
            writeSafe current_user object text

        "read" : object : _ -> do
            readSafe current_user object

        -- grants

        "grant" : right : "for" : "object" : object : "to" : "user" : user : _ ->
            case readMaybe (unpack right) :: Maybe Rights
            of
                Nothing -> pure "can't parse rights"
                Just r  -> grantObjectToUser current_user r object user

        "grant" : right : "for" : "user" : object : "to" : "user" : user : _ ->
            case readMaybe (unpack right) :: Maybe Rights
            of
                Nothing -> pure "can't parse rights"
                Just r  -> grantUserToUser current_user r object user

        "grant" : right : "for" : "user" : object : "to" : "object" : user : _ ->
            case readMaybe (unpack right) :: Maybe Rights
            of
                Nothing -> pure "can't parse rights"
                Just r  -> grantUserToObject current_user r object user

        "grant" : right : "for" : "object" : object : "to" : "object" : user : _ ->
            case readMaybe (unpack right) :: Maybe Rights
            of
                Nothing -> pure "can't parse rights"
                Just r  -> grantObjectToObject current_user r object user

        -- takes

        "take" : right : "to" : "object" : object : "from" : "user" : user : _ ->
            case readMaybe (unpack right) :: Maybe Rights
            of
                Nothing -> pure "can't parse rights"
                Just r  -> takeObjectFromUser current_user r object user

        "take" : right : "to" : "user" : object : "from" : "user" : user : _ ->
            case readMaybe (unpack right) :: Maybe Rights
            of
                Nothing -> pure "can't parse rights"
                Just r  -> takeUserFromUser current_user r object user

        "take" : right : "to" : "user" : object : "from" : "object" : user : _ ->
            case readMaybe (unpack right) :: Maybe Rights
            of
                Nothing -> pure "can't parse rights"
                Just r  -> takeUserFromObject current_user r object user

        "take" : right : "to" : "object" : object : "from" : "object" : user : _ ->
            case readMaybe (unpack right) :: Maybe Rights
            of
                Nothing -> pure "can't parse rights"
                Just r  -> takeObjectFromObject current_user r object user

        -- disposes

        "dispose" : right : "to" : "object" : object : _ ->
            case readMaybe (unpack right) :: Maybe Rights
            of
                Nothing -> pure "can't parse rights"
                Just r  -> disposeToObject current_user r object

        "dispose" : right : "to" : "user" : object : _ ->
            case readMaybe (unpack right) :: Maybe Rights
            of
                Nothing -> pure "can't parse rights"
                Just r  -> disposeToUser current_user r object

        "exit" : [] -> pure ""

        _ -> pure "Incorrect input"


communicate :: Key User -> SqlM ()
communicate user = do
    liftIO . putStr $ "> "
    liftIO . hFlush $ stdout
    str <- liftIO TIO.getLine
    response <- parseStr user str
    liftIO . TIO.putStrLn $ response
    _ <- transactionSave
    if response == ""
    then pure ()
    else communicate user


getUserName :: IO Text
getUserName = do
    putStr $ "Enter name: "
    hFlush stdout
    name <- TIO.getLine
    return name


getPassword :: IO Text
getPassword = do
    putStr $ "Enter your password: "
    hFlush stdout
    pwd <- TIO.getLine
    return pwd


authorize :: Text -> Text -> SqlM (Either Text (Key User))
authorize name pwd = do
    matches <- count $ [ UserName ==. name
                       , UserPassword ==. pwd]
    if matches == 0
        then return . Left $ "Wrong name or password"
        else getUser $ name


main :: IO ()
main = do
    pool <- makeConnectionPool
    runDatabase pool $ do
        doMigrations
        mbAdmin <- createUser "admin" "123"
        case mbAdmin of
          Right admin ->
            forM_ [Read, Write, Take, Grant] $ \right ->
                addRightsNoCheck right admin admin
          _ -> pure ()
        name <- liftIO getUserName
        pwd  <- liftIO getPassword
        user <- checkShould $ authorize name pwd
        communicate $ user

        -- liftIO . putStrLn $ "hello database"
        --
        -- book   <- checkShould $ createObject "kampf" "mein"
        -- add all rights to this book
        -- forM_ [Read, Write, Take, Grant] $ \right -> do
        --     addRightsNoCheck right hitler book
        --
        -- stalin <- checkShould $ createUser "stalin" "stalinpass"
        -- addRightsNoCheck Grant hitler stalin
        --
        -- checkShould $ grant (Read, book) hitler stalin
        -- content <- checkShould $ read stalin book
        -- liftIO $ TIO.putStrLn content
