{-|
Demo executable that parses a orgmode file, inserts the data into the database,
removes all clocks and exports the document as a new orgmode file.
-}

module Main where

import           Database.Persist.Sqlite
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad (void)
import           Control.Monad.Reader (ReaderT)

import           Control.Monad.Logger
import           Control.Monad.Trans.Resource (ResourceT, MonadBaseControl)
import qualified Data.Text.IO as T

import qualified Database.OrgMode as OrgDb
import qualified Database.OrgMode.Query.Clock as Clock
import           Database.OrgMode.Model

-------------------------------------------------------------------------------

main :: IO ()
main = do
    rawInput <- T.readFile "test/examples/all_data.org"
    exportE  <- runDb $ do
        docIdE <- OrgDb.textImportDocument "all_data" ["TODO", "DONE"] rawInput

        case docIdE of
            (Left err)
                -> return (Left err)
            (Right docId)
                -> go docId

    case exportE of
        (Left err)
            -> putStrLn ("Failed with: " ++ err)
        (Right rawOutput)
            -> T.putStrLn rawOutput
  where
    go docId = do
        Clock.deleteAll

        rawExportM <- OrgDb.textExportDocument docId

        return $ case rawExportM of
            Nothing  -> Left "Document was not found in database"
            Just raw -> Right raw

-------------------------------------------------------------------------------
-- * Helpers

setupDb :: (MonadBaseControl IO m, MonadIO m) => ReaderT SqlBackend m ()
setupDb = void $ runMigrationSilent migrateAll

runDb :: forall a. SqlPersistT (NoLoggingT (ResourceT IO)) a -> IO a
runDb a = liftIO $ runSqlite ":memory:" $ setupDb >> a
