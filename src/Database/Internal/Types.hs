{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs, EmptyDataDecls, FlexibleContexts, FlexibleInstances, TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards, TemplateHaskell, QuasiQuotes #-}

module Database.Internal.Types where

-- Description: exports a bunch of template haskell generated names, which for
-- some reason can't be exported exclicitly.


import Data.Text           (Text)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

import Database.Internal.Rights (Rights)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name     Text
    password Text
    deriving Show Eq
Object
    name Text
    body Text
    deriving Show Eq

UserToObjectRight
    prima   User
    secunda Object
    right   Rights
    deriving Show Eq
UserToUserRight
    prima   User
    secunda User
    right   Rights
    deriving Show Eq
ObjectToObjectRight
    prima   Object
    secunda Object
    right   Rights
    deriving Show Eq
ObjectToUserRight
    prima   Object
    secunda User
    right   Rights
    deriving Show Eq
|]
