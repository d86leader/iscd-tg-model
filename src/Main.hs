{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Actions
import Control.Monad          (forM_)
import Control.Monad.Fail     (fail)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid            ((<>))
import Data.Text              (Text, unpack, unwords, words)
import Database.Control
    (SqlM, doMigrations, makeConnectionPool, runDatabase)
import Database.Persist       (count, entityVal, selectFirst, (==.))
import Database.Persist.Sql   (transactionSave)
import Database.Types         (Rights (Read, Write, Take, Grant))
import Database.Types
    (EntityField (ObjectName, UserName, UserPassword), Object, User)
import System.IO              (hFlush, stdout)
import Text.Read              (readMaybe)

import qualified Data.Text.IO as TIO

import Prelude hiding (fail, read, take, unwords, words)

checkShould :: SqlM (Either Text a) -> SqlM a
checkShould v = v >>= \case
    Left message -> (liftIO . TIO.putStrLn) ("Model error: " <> message)
                    >> fail "Aborting due to model error"  -- Maybe it's bad idea to throw error
    Right val    -> pure val

parseStr :: User -> Text -> SqlM Text
parseStr current_user str =
    case words str of
        "create" : "user" : user : password : _ ->
            createUser user password >>= \case
                Left msg  -> pure msg
                Right obj -> do
                    forM_ [Read, Write, Take, Grant] $ \right -> do
                        addRightsNoCheck right current_user obj
                    forM_ [Read, Write, Take, Grant] $ \right -> do
                        addRightsNoCheck right obj obj
                    pure "The user is created"

        "create" : "object" : name : rest -> do
            let text = unwords rest
            createObject name text >>= \case
                Left msg  -> pure msg
                Right obj -> do
                    forM_ [Read, Write, Take, Grant] $ \right -> do
                        addRightsNoCheck right current_user obj
                    pure "The object is created"

        "write" : object : rest -> do
            let text = unwords rest
            getObject object >>= \case
                Left msg  -> pure msg
                Right obj -> 
                    write current_user obj text >>= \case
                        Left msg -> pure msg
                        _ -> pure "Object is updated"

        "read" : object : _ -> do
            getObject object >>= \case
                Left msg  -> pure msg
                Right obj -> 
                    read current_user obj >>= \case
                        Left msg   -> pure msg
                        Right text -> pure text

        -- grants

        "grant" : right : "for" : "object" : object : "to" : "user" : user : _ ->
            case readMaybe (unpack right) :: Maybe Rights
            of
                Nothing -> pure "can't parse rights"
                Just r  -> getObject object >>= \case
                        Left msg  -> pure msg
                        Right obj -> getUser user >>= \case
                                Left msg  -> pure msg
                                Right usr -> 
                                    grant (r, obj)  current_user usr >>= \case
                                        Left msg -> pure msg
                                        _ -> pure "The right is granted"

        "grant" : right : "for" : "user" : object : "to" : "user" : user : _ ->
            case readMaybe (unpack right) :: Maybe Rights
            of
                Nothing -> pure "can't parse rights"
                Just r  -> getUser object >>= \case
                        Left msg  -> pure msg
                        Right obj -> getUser user >>= \case
                                Left msg  -> pure msg
                                Right usr -> 
                                    grant (r, obj)  current_user usr >>= \case
                                        Left msg -> pure msg
                                        _ -> pure "The right is granted"

        "grant" : right : "for" : "user" : object : "to" : "object" : user : _ ->
            case readMaybe (unpack right) :: Maybe Rights
            of
                Nothing -> pure "can't parse rights"
                Just r  -> getUser object >>= \case
                        Left msg  -> pure msg
                        Right obj -> getObject user >>= \case
                                Left msg  -> pure msg
                                Right usr -> 
                                    grant (r, obj)  current_user usr >>= \case
                                        Left msg -> pure msg
                                        _ -> pure "The right is granted"

        "grant" : right : "for" : "object" : object : "to" : "object" : user : _ ->
            case readMaybe (unpack right) :: Maybe Rights
            of
                Nothing -> pure "can't parse rights"
                Just r  -> getObject object >>= \case
                        Left msg  -> pure msg
                        Right obj -> getObject user >>= \case
                                Left msg  -> pure msg
                                Right usr -> 
                                    grant (r, obj)  current_user usr >>= \case
                                        Left msg -> pure msg
                                        _ -> pure "The right is granted"

        -- takes

        "take" : right : "to" : "object" : object : "from" : "user" : user : _ ->
            case readMaybe (unpack right) :: Maybe Rights
            of
                Nothing -> pure "can't parse rights"
                Just r  -> getObject object >>= \case
                    Left msg  -> pure msg
                    Right obj -> getUser user >>= \case
                        Left msg  -> pure msg
                        Right usr ->
                            take (r, obj)  current_user usr >>= \case
                                Left msg -> pure msg
                                _ -> pure "The right is taken"

        "take" : right : "to" : "user" : object : "from" : "user" : user : _ ->
            case readMaybe (unpack right) :: Maybe Rights
            of
                Nothing -> pure "can't parse rights"
                Just r  -> getUser object >>= \case
                    Left msg  -> pure msg
                    Right obj -> getUser user >>= \case
                        Left msg  -> pure msg
                        Right usr ->
                            take (r, obj)  current_user usr >>= \case
                                Left msg -> pure msg
                                _ -> pure "The right is taken"

        "take" : right : "to" : "user" : object : "from" : "object" : user : _ ->
            case readMaybe (unpack right) :: Maybe Rights
            of
                Nothing -> pure "can't parse rights"
                Just r  -> getUser object >>= \case
                    Left msg  -> pure msg
                    Right obj -> getObject user >>= \case
                        Left msg  -> pure msg
                        Right usr ->
                            take (r, obj)  current_user usr >>= \case
                                Left msg -> pure msg
                                _ -> pure "The right is taken"

        "take" : right : "to" : "object" : object : "from" : "object" : user : _ ->
            case readMaybe (unpack right) :: Maybe Rights
            of
                Nothing -> pure "can't parse rights"
                Just r -> getObject object >>= \case
                    Left msg  -> pure msg
                    Right obj -> getObject user >>= \case
                        Left msg  -> pure msg
                        Right usr ->
                            take (r, obj)  current_user usr >>= \case
                                Left msg -> pure msg
                                _ -> pure "The right is taken"

        -- disposes

        "dispose" : right : "to" : "object" : object : _ ->
            case readMaybe (unpack right) :: Maybe Rights
            of
                Nothing -> pure "can't parse rights"
                Just r  -> getObject object >>= \case
                    Left msg  -> pure msg
                    Right obj -> dispose r current_user obj >>= \case
                        Left msg -> pure msg
                        _ -> pure "The right is disposed"

        "dispose" : right : "to" : "user" : object : _ ->
            case readMaybe (unpack right) :: Maybe Rights
            of
                Nothing -> pure "can't parse rights"
                Just r  -> getUser object >>= \case
                    Left msg  -> pure msg
                    Right obj -> dispose r current_user obj >>= \case
                        Left msg -> pure msg
                        _ -> pure "The right is disposed"

        "exit" : [] -> pure ""

        _ -> pure "Incorrect input"


communicate :: User -> SqlM ()
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


getUser :: Text -> SqlM (Either Text User)
getUser name = do
    mbUser <- selectFirst [UserName ==. name] []
    case mbUser of
        Nothing   -> return . Left $ "User with this name doesn't exist"
        Just user -> return . Right . entityVal $ user


getObject :: Text -> SqlM (Either Text Object)
getObject name = do
    mbObject <- selectFirst [ObjectName ==. name] []
    case mbObject of
        Nothing     -> return . Left $ "Object with this name doesn't exist"
        Just object -> return . Right . entityVal $ object


authorize :: Text -> Text -> SqlM (Either Text User)
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
