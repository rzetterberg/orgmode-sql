module TestImport
    ( module TestImport
    , module X
    ) where

import           Test.Hspec              as X
import           Data.Text               as X (Text)
import           Database.Persist.Sqlite as X
import           Control.Monad.IO.Class  as X (liftIO, MonadIO)
import           Control.Monad           as X (void)
import           Control.Monad.Reader    as X (ReaderT)

import           Control.Monad.Logger
import           Control.Monad.Trans.Resource (ResourceT, MonadBaseControl, runResourceT)

import           Database.OrgMode.Model (migrateAll)

-------------------------------------------------------------------------------

setupDb :: (MonadIO m) => ReaderT SqlBackend m ()
setupDb = runMigration migrateAll

runDb :: forall a. SqlPersistT (NoLoggingT (ResourceT IO)) a -> IO a
runDb a = liftIO $ runSqlite ":memory:" $ setupDb >> a

runDb' :: forall a. SqlPersistT (LoggingT (ResourceT IO)) a -> IO a
runDb' a = liftIO $ runSqlite' ":memory:" $ setupDb >> a

runSqlite' :: forall (m :: * -> *) a.
              (MonadIO m, MonadBaseControl IO m) =>
              Text -> SqlPersistT (LoggingT (ResourceT m)) a -> m a
runSqlite' connstr = runResourceT
                   . runStderrLoggingT
                   . withSqliteConn connstr
                   . runSqlConn
