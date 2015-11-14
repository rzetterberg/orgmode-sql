{-|
CRUD functionality for 'Document's.
-}
module Database.OrgMode.Query.Document where

import           Database.Esqueleto
import qualified Database.Persist as P

import           Database.OrgMode.Import
import           Database.OrgMode.Model

-------------------------------------------------------------------------------
-- * Creation

add :: (MonadIO m) => Text -> Text -> ReaderT SqlBackend m (Key Document)
add name text = P.insert (Document name text)

-------------------------------------------------------------------------------
-- * Retrieval

{-|
Retrieves all 'Document's in the database.

ASC sorted by name.
-}
getAll :: (MonadIO m) => ReaderT SqlBackend m [Entity Document]
getAll = P.selectList [] [P.Asc DocumentName]
