module Main where

import Database.Control           (makeConnectionPool, doMigrations, runDatabase)
import Control.Monad.Trans.Class  (lift)
import Control.Monad.IO.Class     (liftIO)

main :: IO ()
main = do
    pool <- makeConnectionPool
    runDatabase pool $ do
        doMigrations
        liftIO . putStrLn $ "hello database"
