{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Database.Control          (makeConnectionPool, doMigrations, runDatabase, SqlM)
import Database.Types            (Rights)
import Data.Monoid               ((<>))
import Control.Monad.Fail        (fail)
import Control.Monad.IO.Class    (liftIO)
import Data.Text                 (Text, words, unwords, unpack)
import qualified Data.Text.IO as TIO
import Actions
import Database.Persist          (entityVal, selectFirst, count, (==.))
import Database.Types            (User, Object, EntityField ( UserName, UserPassword, ObjectName ))
import System.IO                 (hFlush, stdout)
import Prelude hiding            (read, take, fail, words, unwords)
import qualified Prelude

import Database.Persist.Sql (transactionSave)

checkShould :: SqlM (Either Text a) -> SqlM a
checkShould v = v >>= \case
    Left message -> (liftIO . TIO.putStrLn) ("Model error: " <> message)
                    >> fail "Aborting due to model error"  -- Maybe it's bad idea to throw error
    Right val    -> pure val

parseStr :: User -> Text -> SqlM Text
parseStr current_user str =
    case words str of
        "create" : "user" : user : password : _ -> do
            _ <- checkShould $ createUser user password
            pure "The user is created"
        
        "create" : "object" : name : rest -> do
            let text = unwords rest 
            _ <- checkShould $ createObject name text 
            pure "The object is created"
        
        "write" : object : rest -> do
            obj <- checkShould $ getObject object
            let text = unwords rest
            _ <- checkShould $ write current_user obj text
            pure "Object is updated"
        
        "read" : object : _ -> do
            obj <- checkShould $ getObject object 
            checkShould $ read current_user obj 
        
        "grant" : right : "for" : "object" : object : "to" : user : _ -> do
            let r = Prelude.read (unpack right) :: Rights -- "Prelude.read: no parse" if not Read, Write, ect.
            obj <- checkShould $ getObject object
            usr <- checkShould $ getUser user
            _ <- checkShould $ grant (r, obj)  current_user usr
            pure "The right is granted"
        
        "grant" : right : "for" : "user" : object : "to" : user : _ -> do
            let r = Prelude.read (unpack right) :: Rights -- "Prelude.read: no parse" if not Read, Write, ect.
            obj <- checkShould $ getUser object
            usr <- checkShould $ getUser user
            _ <- checkShould $ grant (r, obj)  current_user usr
            pure "The right is granted"

        "take" : right : "to" : "object" : object : "from" : user : _ -> do 
            let r = Prelude.read (unpack right) :: Rights -- "Prelude.read: no parse" if not Read, Write, ect.
            obj <- checkShould $ getObject object
            usr <- checkShould $ getUser user
            _ <- checkShould $ take (r, obj)  current_user usr
            pure "The right is taken"

        "take" : right : "to" : "user" : object : "from" : user : _ -> do 
            let r = Prelude.read (unpack right) :: Rights -- "Prelude.read: no parse" if not Read, Write, ect.
            obj <- checkShould $ getUser object
            usr <- checkShould $ getUser user
            _ <- checkShould $ take (r, obj)  current_user usr
            pure "The right is taken"

        "dispose" : right : "to" : "object" : object : _ -> do
            let r = Prelude.read (unpack right) :: Rights -- "Prelude.read: no parse" if not Read, Write, ect.
            obj <- checkShould $ getObject object
            _ <- checkShould $ dispose r current_user obj
            pure "The right is disposed"
        
        "dispose" : right : "to" : "user" : object : _ -> do
            let r = Prelude.read (unpack right) :: Rights -- "Prelude.read: no parse" if not Read, Write, ect.
            obj <- checkShould $ getUser object
            _ <- checkShould $ dispose r current_user obj
            pure "The right is disposed"
       
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
        name <- liftIO getUserName
        pwd  <- liftIO getPassword
        user <- checkShould $ authorize name pwd
        communicate $ user

        -- liftIO . putStrLn $ "hello database"
        --
        -- hitler <- checkShould $ createUser "hitler" "hitlerpass"
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
