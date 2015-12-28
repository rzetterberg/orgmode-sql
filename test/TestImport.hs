module TestImport
    ( module TestImport
    , module X
    ) where

import           Test.Hspec              as X
import           Data.Text               as X (Text)
import           Database.Persist.Sqlite as X
import           Control.Monad.IO.Class  as X (liftIO, MonadIO)
import           Control.Monad           as X (void, liftM)
import           Control.Monad.Reader    as X (ReaderT)

import           Control.Monad.Logger
import           Control.Monad.Trans.Resource (ResourceT, MonadBaseControl, runResourceT)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Database.OrgMode as Db
import qualified Database.OrgMode.Internal.Types as Db

-------------------------------------------------------------------------------

setupDb :: (MonadBaseControl IO m, MonadIO m) => ReaderT SqlBackend m ()
setupDb = void $ runMigrationSilent Db.migrateAll

runDb :: forall a. SqlPersistT (NoLoggingT (ResourceT IO)) a -> IO a
runDb a = liftIO $ runSqlite ":memory:" $ setupDb >> a

runDb' :: forall a. SqlPersistT (LoggingT (ResourceT IO)) a -> IO a
runDb' a = liftIO $ runSqlite' ":memory:" $ setupDb >> a

runSqlite' :: forall (m :: * -> *) a.
              (MonadIO m, MonadBaseControl IO m)
           => Text
           -> SqlPersistT (LoggingT (ResourceT m)) a
           -> m a
runSqlite' connstr = runResourceT
                   . runStderrLoggingT
                   . withSqliteConn connstr
                   . runSqlConn

{-|
Imports the given example file to the database
-}
importExample :: (MonadIO m)
              => FilePath
              -> ReaderT SqlBackend m (Key Db.Document)
importExample fname = do
    contents <- liftIO (getExample fname)
    parseImport (T.pack fname) allowedTags contents
  where
    allowedTags = ["TODO", "DONE"]

{-|
Helper for reading a org-mode example file from the `examples` directory in
test.
-}
getExample :: FilePath -> IO Text
getExample fname = T.readFile $ "test/examples/" ++ fname

{-|
Helper for parsing org-mode document contents and then importing the result
into the database. Does nothing on parsing failure, instead lets the database
test constraints handle the failure (since nothing will be inserted into the
database on parse failure).
-}
parseImport :: (MonadIO m)
            => Text                    -- ^ Name of the document
            -> [Text]                  -- ^ Keywords to allow
            -> Text                    -- ^ org-mode document contents
            -> ReaderT SqlBackend m (Key Db.Document)
parseImport docName keywords contents
    = getId `liftM` Db.textImportDocument docName keywords contents
  where
    getId (Right docId) = docId
    getId (Left err)    = error err
