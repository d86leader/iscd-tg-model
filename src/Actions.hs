{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
module Actions
( Entity (..)
, addRightsNoCheck
, createUser, createObject
, write, read
, checkRights, take, grant, dispose
) where

-- Description: actions you can perform on entities in database


import Database.Control          (SqlM)
import Database.Persist          ( selectList, count, insert, deleteWhere, updateWhere
                                 , (==.), (=.)
                                 , Key, entityKey, entityVal
                                 , Filter
                                 )
import Data.Text                 (Text)

import Database.Types
    ( User (User), Object (Object, objectName, objectBody)
    , UserToObjectRight   (UserToObjectRight, userToObjectRightRight)
    , UserToUserRight     (UserToUserRight, userToUserRightRight)
    , ObjectToObjectRight (ObjectToObjectRight)
    , ObjectToUserRight   (ObjectToUserRight)
    , Rights (Read, Write, Take, Grant)
    , EntityField ( UserName, ObjectName, ObjectBody
                  , UserToUserRightPrima, UserToUserRightSecunda, UserToUserRightRight
                  , UserToObjectRightPrima, UserToObjectRightSecunda, UserToObjectRightRight
                  )
    )
import Data.Proxy (Proxy (Proxy))
import Database.Persist.Class (PersistEntityBackend, PersistEntity, get)
import Database.Persist.Sql (SqlBackend)

import Prelude hiding (take, read)


-- | Actions should make sence for both users and objects
data Entity = EUser User | EObject Object

-- | Actions can fail to complete because of error
type Should a = Either Text a


-- Simple creation routines


createUser :: Text -> SqlM (Should User)
createUser name = do
    matches <- count $ [UserName ==. name]
    if matches /= 0
      then return . Left $ "User with this name exists"
      else do
        key <- insert $ User name
        maybeUser <- get key
        case maybeUser of Nothing -> return . Left $ "Something went very wrong"
                          Just user -> return . Right $ user

createObject :: Text -> Text -> SqlM (Should Object)
createObject name content = do
    matches <- count $ [ObjectName ==. name]
    if matches /= 0
      then return . Left $ "Object with this name exists"
      else do
        key <- insert $ Object name content
        maybeObj <- get key
        case maybeObj of Nothing -> return . Left $ "Something went very wrong"
                         Just obj -> return . Right $ obj


-- Object actions


write :: User -> Object -> Text -> SqlM (Should ())
write user object text = do
    rights <- checkRights user object
    if Write `elem` rights
      then updateWhere [ObjectName ==. objectName object]
                       [ObjectBody =. text]
           >> return (Right ())
      else return . Left $ "No right to write"


read :: User -> Object -> SqlM (Should Text)
read user object = do
    rights <- checkRights user object
    if Read `elem` rights
      then return . Right . objectBody $ object
      else return . Left $ "No right to read"


-- Meta-rights functions


class (PersistEntity r, PersistEntityBackend r ~ SqlBackend) => Related p s r | p s -> r where
    relation       :: p -> s -> Rights -> r
    filtersPrima   :: Proxy (p, s) -> p -> Filter r
    filtersSecunda :: Proxy (p, s) -> s -> Filter r
    filtersRight   :: Proxy (p, s) -> Rights -> Filter r
    extractRight   :: Proxy (p, s) -> r -> Rights

instance Related User User   UserToUserRight where
    relation       =         UserToUserRight
    filtersPrima   _ = (==.) UserToUserRightPrima
    filtersSecunda _ = (==.) UserToUserRightSecunda
    filtersRight   _ = (==.) UserToUserRightRight
    extractRight   _ =       userToUserRightRight

instance Related User Object UserToObjectRight where
    relation       =         UserToObjectRight
    filtersPrima   _ = (==.) UserToObjectRightPrima
    filtersSecunda _ = (==.) UserToObjectRightSecunda
    filtersRight   _ = (==.) UserToObjectRightRight
    extractRight   _ =       userToObjectRightRight

asProxy :: a -> Proxy a
asProxy = const Proxy


-- The following functions can be invoked with User or Object in place of any argument


addRightsNoCheck :: (Related p s r) => Rights -> p -> s -> SqlM ()
addRightsNoCheck right prima secunda =
    insert (relation prima secunda right) >> return ()


checkRights :: (Related p s r) => p -> s -> SqlM [Rights]
checkRights prima secunda = do
    let proxy = asProxy (prima, secunda)
    rels <- selectList [filtersPrima proxy prima, filtersSecunda proxy secunda] []
    pure . map (extractRight proxy . entityVal) $ rels


grant :: (Related granter grantee r1, Related granter object r2, Related grantee object r3)
      => (Rights, object) -> granter -> grantee -> SqlM (Should ())
grant (right, obj) u1 u2 = do
    let proxy1 = asProxy (u1, obj)
    hasRight <- count [ filtersPrima   proxy1 u1
                      , filtersSecunda proxy1 obj
                      , filtersRight   proxy1 right
                      ]
    let proxy2 = asProxy (u1, u2)
    hasGrant <- count [ filtersPrima   proxy2 u1
                      , filtersSecunda proxy2 u2
                      , filtersRight   proxy2 Grant
                      ]
    if | hasRight == 0 -> return . Left $ "Can't grant right you don't have"
       | hasGrant == 0 -> return . Left $ "Can't grant right without grant"
       | otherwise -> insert (relation u2 obj right)
                      >> return (Right ())


take :: (Related taker takee r1, Related takee object r2, Related taker object r3)
     => (Rights, object) -> taker -> takee -> SqlM (Should ())
take (right, obj) u1 u2 = do
    let proxy1 = asProxy (u2, obj)
    hasRight <- count [ filtersPrima   proxy1 u2
                      , filtersSecunda proxy1 obj
                      , filtersRight   proxy1 right
                      ]
    let proxy2 = asProxy (u1, u2)
    hasTake  <- count [ filtersPrima   proxy2 u1
                      , filtersSecunda proxy2 u2
                      , filtersRight   proxy2 Take
                      ]
    if | hasRight == 0 -> return . Left $ "Can't take right they don't have"
       | hasTake  == 0 -> return . Left $ "Can't take right without take"
       | otherwise -> insert (relation u1 obj right)
                      >> return (Right ())

dispose :: (Related subject item r)
        => Rights -> subject -> item -> SqlM (Should ())
dispose right subject item = do
    let proxy = asProxy (subject, item)
    let pattern = [ filtersPrima   proxy subject
                  , filtersSecunda proxy item
                  , filtersRight   proxy right
                  ]
    hasRight <- count pattern
    if hasRight == 0 then return . Left $ "Can't dispose of right you don't have"
                     else deleteWhere pattern >> return (Right ())
