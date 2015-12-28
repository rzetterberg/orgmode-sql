{-|
Functionality to export different parts of the orgmode data structure tree.
All parts have functions to export them separately.
-}

module Database.OrgMode.Export.Text where

import           Database.OrgMode.Export.OrgParse (exportDocument)
import           Database.OrgMode.Internal.Convert.OrgParse (toText)
import           Database.OrgMode.Internal.Import
import qualified Database.OrgMode.Internal.Types as Db

-------------------------------------------------------------------------------

{-|
Exports all data from the database to a plain text org-mode document as a strict
'Text'.
-}
textExportDocument :: (MonadIO m)
                   => Key Db.Document
                   -> ReaderT SqlBackend m (Maybe Text)
textExportDocument docId
    = (fmap toText) `liftM` exportDocument docId
