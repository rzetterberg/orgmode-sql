{-|
CRUD functionality for 'Heading's.
-}
module Database.OrgMode.Model.Heading where

import qualified Database.Persist as P

import           Database.OrgMode.Import
import           Database.OrgMode.Model

-------------------------------------------------------------------------------
-- * Creation

{-|
Adds the given 'Heading' into the database. Simply a wrapper for persists'
'insert' function.
-}
add :: (MonadIO m) => Heading -> ReaderT SqlBackend m (Key Heading)
add = P.insert

-------------------------------------------------------------------------------
-- * Retrieval

{-|
Retrieves all 'Heading's in the database.

ASC sorted by title.
-}
getAll :: (MonadIO m) => ReaderT SqlBackend m [Entity Heading]
getAll = P.selectList [] [P.Asc HeadingTitle]
