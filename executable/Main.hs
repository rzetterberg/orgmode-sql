{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Main where

import           Database.Persist.Sqlite
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad (void)

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Database.OrgMode as OrgDb
import qualified Database.OrgMode.Query.Clock as Clock
import           Database.OrgMode.Types (migrateAll)

-------------------------------------------------------------------------------

testDoc :: Text
testDoc = T.intercalate "\n"
    [ "* TODO Make more examples :documentation:"
    , "  CLOCK: [2015-10-08 Thu 16:24]--[2015-10-08 Thu 17:10] =>  0:46"
    ]

main :: IO ()
main = liftIO $ runSqlite ":memory:" $ do
    liftIO $ do
        T.putStrLn ">> Plain text input:"
        T.putStrLn "---"
        T.putStrLn testDoc
        T.putStrLn "---"

    -- Create the database schema
    void $ runMigrationSilent migrateAll

    -- Parse and import our test document and name the document "test_doc"
    docIdE <- OrgDb.textImportDocument "test_doc" ["TODO", "DONE"] testDoc

    -- Check if import was successful,
    docId <- case docIdE of
        (Left err)    -> error $ "Parsing failed: " ++ err
        (Right docId) -> return docId

    -- Remove the all clocks from the database
    Clock.deleteAll

    -- Export the document as plain text orgmode data
    outputM <- OrgDb.textExportDocument docId

    -- Check that the imported document was found in the database
    liftIO $ case outputM of
        Nothing       -> T.putStrLn ">> Document not found in database"
        (Just output) -> do
            T.putStrLn ">> Plain text output after removing clocks:"
            T.putStrLn "---"
            T.putStrLn output
            T.putStrLn "---"
