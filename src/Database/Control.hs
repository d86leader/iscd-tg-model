{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Database.Control
( SqlM, runDatabase
, doMigrations
, ConnectionPool, makeConnectionPool
) where

import Database.Persist.Sql       (SqlPersistT, runMigration, runSqlPool)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.Logger       (MonadLogger, monadLoggerLog, toLogStr)
import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import System.IO                  (hPutStrLn, hPutStr, hFlush, stderr)
import Database.Types             (migrateAll)

import Database.Persist.Sqlite (ConnectionPool, createSqlitePool)


-- | Monad to execute database actions in
type SqlM a = ReaderT ConnectionPool (SqlPersistT IO) a


-- | For running database actions in IO mainly
runDatabase :: MonadIO m => ConnectionPool -> SqlM a -> m a
runDatabase pool computation = do
    let query = runReaderT computation pool
    liftIO $ runSqlPool query pool

doMigrations :: SqlM ()
doMigrations = do
    pool <- ask
    lift $ runSqlPool (runMigration migrateAll) pool


makeConnectionPool :: IO ConnectionPool
makeConnectionPool = createSqlitePool connString connCount
    where connString = "test.sqlite3"
          connCount = 10

-- | Trivial logger for database, required for opening the connection pool
instance MonadLogger IO where
    monadLoggerLog _ _ _ msg = do
        hPutStr stderr "Database says: "
        hPutStrLn stderr . show . toLogStr $ msg
        hFlush stderr

