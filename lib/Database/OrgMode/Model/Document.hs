{-|
CRUD functionality for 'Document's.
-}
module Database.OrgMode.Model.Document where

import           Database.Persist

import           Database.OrgMode.Import
import           Database.OrgMode.Model

-------------------------------------------------------------------------------
-- * Creation

{-|
Adds the given 'Document' into the database. Simply a wrapper for persists'
'insert' function.
-}
add :: (MonadIO m) => Document -> ReaderT SqlBackend m (Key Document)
add = insert

-------------------------------------------------------------------------------
-- * Retrieval

{-|
Retrieves all 'Document's in the database.

ASC sorted by name.
-}
getAll :: (MonadIO m) => ReaderT SqlBackend m [Entity Document]
getAll = selectList [] [Asc DocumentName]
