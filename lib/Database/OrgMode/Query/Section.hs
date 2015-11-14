{-|
CRUD functionality for 'Section's.
-}
module Database.OrgMode.Query.Section where

import           Database.Persist

import           Database.OrgMode.Import
import           Database.OrgMode.Model

-------------------------------------------------------------------------------
-- * Creation

{-|
Adds the given 'Section' into the database. Simply a wrapper for persists'
'insert' function.
-}
add :: (MonadIO m) => Section -> ReaderT SqlBackend m (Key Section)
add = insert

-------------------------------------------------------------------------------
-- * Retrieval

{-|
Retrieves all 'Section's in the database.

No sorting
-}
getAll :: (MonadIO m) => ReaderT SqlBackend m [Entity Section]
getAll = selectList [] []
