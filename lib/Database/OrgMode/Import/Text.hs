{-|
Functionality to import different parts of the orgmode data structure tree.
All parts have functions to import them separately.
-}

module Database.OrgMode.Import.Text where

import           Data.Attoparsec.Text (parseOnly)
import           Data.Text (append)
import qualified Data.OrgMode.Parse.Attoparsec.Document as OrgParse

import           Database.OrgMode.Import.OrgParse (importDocument)
import           Database.OrgMode.Internal.Import
import qualified Database.OrgMode.Internal.Types as Db

-------------------------------------------------------------------------------
-- * Plain text

{-|
Takes a strict 'Text' and tries to parse the document and import it to the
database.

Returns the document ID of the created document or the error message from
the parser.
-}
textImportDocument :: (MonadIO m)
                   => Text                    -- ^ Name of the document
                   -> [Text]                  -- ^ Keywords to allow
                   -> Text                    -- ^ org-mode document contents
                   -> ReaderT SqlBackend m (Either String (Key Db.Document))
textImportDocument docName keywords orgContent =
    case result of
        Left  err -> return (Left err)
        Right doc -> Right `liftM` importDocument docName doc
  where
    result = parseOnly (OrgParse.parseDocument keywords)
                       (append orgContent "\n")
