{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Requests where

import Actions
import Control.Monad          (forM_)
import Data.Text              (Text)
import Database.Control       (SqlM)
import Database.Persist       (entityKey, selectFirst, (==.), Key)
import Database.Types
    ( EntityField (ObjectName, UserName), Object, User
    , Rights (Read, Write, Take, Grant)
    )

import Prelude hiding (fail, read, take, unwords, words)


getUser :: Text -> SqlM (Either Text (Key User))
getUser name = do
    mbUser <- selectFirst [UserName ==. name] []
    case mbUser of
        Nothing   -> return . Left $ "User with this name doesn't exist"
        Just user -> return . Right . entityKey $ user


getObject :: Text -> SqlM (Either Text (Key Object))
getObject name = do
    mbObject <- selectFirst [ObjectName ==. name] []
    case mbObject of
        Nothing     -> return . Left $ "Object with this name doesn't exist"
        Just object -> return . Right . entityKey $ object


createUserSafe :: Key User -> Text -> Text -> SqlM Text
createUserSafe current_user user password =
    createUser user password >>= \case
        Left msg  -> pure msg
        Right obj -> do
            forM_ [Read, Write, Take, Grant] $ \right -> do
                addRightsNoCheck right current_user obj
            forM_ [Read, Write, Take, Grant] $ \right -> do
                addRightsNoCheck right obj obj
            pure "The user is created"


createObjectSafe :: Key User -> Text -> Text -> SqlM Text
createObjectSafe current_user object text = 
    createObject object text >>= \case
        Left msg  -> pure msg
        Right obj -> do
            forM_ [Read, Write, Take, Grant] $ \right -> do
                addRightsNoCheck right current_user obj
            pure "The object is created"


writeSafe :: Key User -> Text -> Text -> SqlM Text
writeSafe current_user object text =
    getObject object >>= \case
        Left msg  -> pure msg
        Right obj -> 
            write current_user obj text >>= \case
                Left msg -> pure msg
                _ -> pure "Object is updated"


readSafe :: Key User -> Text -> SqlM Text
readSafe current_user object =
    getObject object >>= \case
        Left msg  -> pure msg
        Right obj -> 
            read current_user obj >>= \case
                Left msg   -> pure msg
                Right text -> pure text


grantObjectToUser :: Key User -> Rights -> Text -> Text -> SqlM Text
grantObjectToUser current_user right object user =
    getObject object >>= \case
        Left msg  -> pure msg
        Right obj -> getUser user >>= \case
            Left msg  -> pure msg
            Right usr -> grant (right, obj)  current_user usr >>= \case
                Left msg -> pure msg
                _ -> pure "The right is granted"


grantUserToUser :: Key User -> Rights -> Text -> Text -> SqlM Text
grantUserToUser current_user right object user =
    getUser object >>= \case
        Left msg  -> pure msg
        Right obj -> getUser user >>= \case
            Left msg  -> pure msg
            Right usr -> grant (right, obj)  current_user usr >>= \case
                Left msg -> pure msg
                _ -> pure "The right is granted"


grantUserToObject :: Key User -> Rights -> Text -> Text -> SqlM Text
grantUserToObject current_user right object user =
    getUser object >>= \case
        Left msg  -> pure msg
        Right obj -> getObject user >>= \case
            Left msg  -> pure msg
            Right usr -> grant (right, obj)  current_user usr >>= \case
                Left msg -> pure msg
                _ -> pure "The right is granted"


grantObjectToObject :: Key User -> Rights -> Text -> Text -> SqlM Text
grantObjectToObject current_user right object user =
    getObject object >>= \case
        Left msg  -> pure msg
        Right obj -> getObject user >>= \case
            Left msg  -> pure msg
            Right usr -> grant (right, obj)  current_user usr >>= \case
                Left msg -> pure msg
                _ -> pure "The right is granted"


takeUserFromUser :: Key User -> Rights -> Text -> Text -> SqlM Text
takeUserFromUser current_user right object user =
    getUser object >>= \case
        Left msg  -> pure msg
        Right obj -> getUser user >>= \case
            Left msg  -> pure msg
            Right usr -> take (right, obj)  current_user usr >>= \case
                Left msg -> pure msg
                _ -> pure "The right is taken"


takeUserFromObject :: Key User -> Rights -> Text -> Text -> SqlM Text
takeUserFromObject current_user right object user =
    getUser object >>= \case
        Left msg  -> pure msg
        Right obj -> getObject user >>= \case
            Left msg  -> pure msg
            Right usr -> take (right, obj)  current_user usr >>= \case
                Left msg -> pure msg
                _ -> pure "The right is taken"


takeObjectFromUser :: Key User -> Rights -> Text -> Text -> SqlM Text
takeObjectFromUser current_user right object user =
    getObject object >>= \case
        Left msg  -> pure msg
        Right obj -> getUser user >>= \case
            Left msg  -> pure msg
            Right usr -> take (right, obj)  current_user usr >>= \case
                Left msg -> pure msg
                _ -> pure "The right is taken"


takeObjectFromObject :: Key User -> Rights -> Text -> Text -> SqlM Text
takeObjectFromObject current_user right object user =
    getObject object >>= \case
        Left msg  -> pure msg
        Right obj -> getObject user >>= \case
            Left msg  -> pure msg
            Right usr -> take (right, obj)  current_user usr >>= \case
                Left msg -> pure msg
                _ -> pure "The right is taken"


disposeToUser :: Key User -> Rights -> Text -> SqlM Text
disposeToUser current_user right object = 
    getUser object >>= \case
        Left msg  -> pure msg
        Right obj -> dispose right current_user obj >>= \case
            Left msg -> pure msg
            _ -> pure "The right is disposed"

disposeToObject :: Key User -> Rights -> Text -> SqlM Text
disposeToObject current_user right object = 
    getObject object >>= \case
        Left msg  -> pure msg
        Right obj -> dispose right current_user obj >>= \case
            Left msg -> pure msg
            _ -> pure "The right is disposed"

