{-# LANGUAGE OverloadedStrings #-}
module Database.Internal.Rights
( Rights (..)
) where

-- Description: this module is specifically for using Rights when defining our
-- database types. For some reason GHC forbids us from defining Rights in the
-- same module as all other types

import Database.Persist.Class (PersistField (toPersistValue, fromPersistValue))
import Database.Persist.Sql   (PersistFieldSql (sqlType), SqlType (SqlInt64))
import Database.Persist.Types (PersistValue (PersistInt64))


-- | What rights an entity can have to an entity
data Rights = Read | Write | Take | Grant
    deriving (Eq, Show, Read, Enum)

instance PersistField Rights where
    toPersistValue = PersistInt64 . fromIntegral . fromEnum
    fromPersistValue (PersistInt64 x) = Right . toEnum $ fromIntegral x
    fromPersistValue _ = Left "Expected integral value"

instance PersistFieldSql Rights where
    sqlType _ = SqlInt64
