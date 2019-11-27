{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Database.Control          (makeConnectionPool, doMigrations, runDatabase, SqlM)
import Database.Types            (Rights (Write, Read, Take, Grant))
import Data.Monoid               ((<>))
import Control.Monad             (forM_)
import Control.Monad.Fail        (fail)
import Control.Monad.IO.Class    (liftIO)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Actions

import Prelude hiding (read, take, fail)

checkShould :: SqlM (Either Text a) -> SqlM a
checkShould v = v >>= \case
    Left message -> (liftIO . TIO.putStrLn) ("Model error: " <> message)
                    >> fail "Aborting due to model error"
    Right val    -> pure val

main :: IO ()
main = do
    pool <- makeConnectionPool
    runDatabase pool $ do
        doMigrations
        liftIO . putStrLn $ "hello database"
        --
        hitler <- checkShould $ createUser "hitler"
        book   <- checkShould $ createObject "kampf" "mein"
        -- add all rights to this book
        forM_ [Read, Write, Take, Grant] $ \right -> do
            addRightsNoCheck right hitler book
        --
        stalin <- checkShould $ createUser "stalin"
        addRightsNoCheck Grant hitler stalin
        --
        checkShould $ grant (Read, book) hitler stalin
        content <- checkShould $ read stalin book
        liftIO $ TIO.putStrLn content

