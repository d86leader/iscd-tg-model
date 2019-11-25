{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Actions
( Entity (..)
, createUser, createObject
, write, read
, checkRights, take, grant, dispose
) where

-- Description: actions you can perform on entities in database


import Database.Control          (SqlM)
import Database.Persist          ( selectList, count, insert, deleteWhere, updateWhere
                                 , (==.), (=.)
                                 , Key, entityKey, entityVal
                                 )
import Data.Text                 (Text)

import Database.Types
    ( User (User), Object (Object, objectName, objectBody)
    , UserToObjectRight   (UserToObjectRight)
    , UserToUserRight     (UserToUserRight, userToUserRightRight)
    , ObjectToObjectRight (ObjectToObjectRight)
    , ObjectToUserRight   (ObjectToUserRight)
    , Rights (Read, Write, Take, Grant)
    , EntityField ( UserName, ObjectName, ObjectBody
                  , UserToUserRightPrima, UserToUserRightSecunda, UserToUserRightRight
                  )
    )

import Prelude hiding (take, read)


-- | Actions should make sence for both users and objects
data Entity = EUser User | EObject Object

-- | Actions can fail to complete because of error
type Should a = Either Text a


-- Simple creation routines


createUser :: Text -> SqlM (Should (Key User))
createUser name = do
    matches <- count $ [UserName ==. name]
    if matches /= 0
      then return . Left $ "User with this name exists"
      else fmap Right . insert $ User name

createObject :: Text -> Text -> SqlM (Should (Key Object))
createObject name content = do
    matches <- count $ [ObjectName ==. name]
    if matches /= 0
      then return . Left $ "Object with this name exists"
      else fmap Right . insert $ Object name content


-- Object actions


write :: User -> Object -> Text -> SqlM (Should ())
write user object text = do
    rights <- checkRights (EUser user) (EObject object)
    if Write `elem` rights
      then updateWhere [ObjectName ==. objectName object]
                       [ObjectBody =. text]
           >> return (Right ())
      else return . Left $ "No right to write"


read :: User -> Object -> SqlM (Should Text)
read user object = do
    rights <- checkRights (EUser user) (EObject object)
    if Read `elem` rights
      then return . Right . objectBody $ object
      else return . Left $ "No right to read"


-- Meta-rights functions


checkRights :: Entity -> Entity -> SqlM [Rights]
checkRights (EUser u1) (EUser u2) = do
    rels <- selectList [UserToUserRightPrima ==. u1, UserToUserRightSecunda ==. u2] []
    pure . map (userToUserRightRight . entityVal) $ rels


grant :: (Rights, Entity) -> Entity -> Entity -> SqlM (Should ())
grant (right, EUser obj) (EUser u1) (EUser u2) = do
    hasRight <- count [ UserToUserRightPrima   ==. u1
                      , UserToUserRightSecunda ==. obj
                      , UserToUserRightRight   ==. right
                      ]
    hasGrant <- count [ UserToUserRightPrima   ==. u1
                      , UserToUserRightSecunda ==. u2
                      , UserToUserRightRight   ==. Grant
                      ]
    if | hasRight == 0 -> return . Left $ "Can't grant right you don't have"
       | hasGrant == 0 -> return . Left $ "Can't grant right without grant"
       | otherwise -> insert (UserToUserRight u2 obj right)
                      >> return (Right ())


take :: (Rights, Entity) -> Entity -> Entity -> SqlM (Should ())
take (right, EUser obj) (EUser u1) (EUser u2) = do
    hasRight <- count [ UserToUserRightPrima   ==. u2
                      , UserToUserRightSecunda ==. obj
                      , UserToUserRightRight   ==. right
                      ]
    hasTake  <- count [ UserToUserRightPrima   ==. u1
                      , UserToUserRightSecunda ==. u2
                      , UserToUserRightRight   ==. Take
                      ]
    if | hasRight == 0 -> return . Left $ "Can't take right they don't have"
       | hasTake  == 0 -> return . Left $ "Can't take right without take"
       | otherwise -> insert (UserToUserRight u1 obj right)
                      >> return (Right ())


dispose :: Rights -> Entity -> Entity -> SqlM (Should ())
dispose right (EUser u1) (EUser u2) = do
    let pattern = [ UserToUserRightPrima   ==. u1
                  , UserToUserRightSecunda ==. u2
                  , UserToUserRightRight   ==. right
                  ]
    hasRight <- count pattern
    if hasRight == 0 then return . Left $ "Can't dispose of right you don't have"
                     else deleteWhere pattern >> return (Right ())
