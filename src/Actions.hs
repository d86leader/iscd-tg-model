{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeFamilies           #-}
module Actions
( Entity (..)
, addRightsNoCheck
, createUser, createObject
, write, read
, checkRights, take, grant, dispose
) where

-- Description: actions you can perform on entities in database


import Database.Control (SqlM)
import Database.Persist ( selectList, count, insert, deleteWhere, updateWhere
                        , (==.), (=.)
                        , entityVal
                        , Filter
                        , Key
                        )
import Data.Text        (Text)

import Database.Types
    ( User (User), Object (Object, objectBody)
    , UserToObjectRight   (UserToObjectRight, userToObjectRightRight)
    , UserToUserRight     (UserToUserRight, userToUserRightRight)
    , ObjectToUserRight   (ObjectToUserRight, objectToUserRightRight)
    , ObjectToObjectRight (ObjectToObjectRight, objectToObjectRightRight)
    , ObjectToObjectRight (ObjectToObjectRight)
    , ObjectToUserRight   (ObjectToUserRight)
    , Rights (Read, Write, Take, Grant)
    , EntityField ( UserName, ObjectName, ObjectBody, ObjectId
                  , UserToUserRightPrima, UserToUserRightSecunda, UserToUserRightRight
                  , UserToObjectRightPrima, UserToObjectRightSecunda, UserToObjectRightRight
                  , ObjectToObjectRightPrima, ObjectToObjectRightSecunda, ObjectToObjectRightRight
                  , ObjectToUserRightPrima, ObjectToUserRightSecunda, ObjectToUserRightRight
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


createUser :: Text -> Text -> SqlM (Should (Key User))
createUser name password = do
    matches <- count $ [UserName ==. name]
    if matches /= 0
      then return . Left $ "User with this name exists"
      else do
        fmap Right . insert $ User name password

createObject :: Text -> Text -> SqlM (Should (Key Object))
createObject name content = do
    matches <- count $ [ObjectName ==. name]
    if matches /= 0
      then return . Left $ "Object with this name exists"
      else do
        fmap Right . insert $ Object name content


-- Object actions


write :: Key User -> Key Object -> Text -> SqlM (Should ())
write user object text = do
    rights <- checkRights user object
    if Write `elem` rights
      then updateWhere [ObjectId ==. object]
                       [ObjectBody =. text]
           >> return (Right ())
      else return . Left $ "No right to write"


read :: Key User -> Key Object -> SqlM (Should Text)
read user object = do
    rights <- checkRights user object
    if Read `elem` rights
      then get object >>= \case
        Just obj -> pure . Right . objectBody $ obj
        Nothing  -> pure . Right $
            "Attempting to access deleted object (possibly a race)"
      else return . Left $ "No right to read"


-- Meta-rights functions


class (PersistEntity r, PersistEntityBackend r ~ SqlBackend) => Related p s r | p s -> r where
    relation       :: p -> s -> Rights -> r
    filtersPrima   :: Proxy (p, s) -> p -> Filter r
    filtersSecunda :: Proxy (p, s) -> s -> Filter r
    filtersRight   :: Proxy (p, s) -> Rights -> Filter r
    extractRight   :: Proxy (p, s) -> r -> Rights

instance Related (Key User) (Key User) UserToUserRight where
    relation       =           UserToUserRight
    filtersPrima   _ = (==.)   UserToUserRightPrima
    filtersSecunda _ = (==.)   UserToUserRightSecunda
    filtersRight   _ = (==.)   UserToUserRightRight
    extractRight   _ =         userToUserRightRight

instance Related (Key User) (Key Object) UserToObjectRight where
    relation       =             UserToObjectRight
    filtersPrima   _ = (==.)     UserToObjectRightPrima
    filtersSecunda _ = (==.)     UserToObjectRightSecunda
    filtersRight   _ = (==.)     UserToObjectRightRight
    extractRight   _ =           userToObjectRightRight

instance Related (Key Object) (Key User) ObjectToUserRight where
    relation       =             ObjectToUserRight
    filtersPrima   _ = (==.)     ObjectToUserRightPrima
    filtersSecunda _ = (==.)     ObjectToUserRightSecunda
    filtersRight   _ = (==.)     ObjectToUserRightRight
    extractRight   _ =           objectToUserRightRight

instance Related (Key Object) (Key Object) ObjectToObjectRight where
    relation       =               ObjectToObjectRight
    filtersPrima   _ = (==.)       ObjectToObjectRightPrima
    filtersSecunda _ = (==.)       ObjectToObjectRightSecunda
    filtersRight   _ = (==.)       ObjectToObjectRightRight
    extractRight   _ =             objectToObjectRightRight

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
