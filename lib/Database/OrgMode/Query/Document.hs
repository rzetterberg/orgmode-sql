{-|
CRUD functionality for 'Document's.
-}
module Database.OrgMode.Query.Document where

import           Database.Esqueleto
import qualified Database.Persist as P

import           Database.OrgMode.Internal.Import
import           Database.OrgMode.Types

-------------------------------------------------------------------------------
-- * Creation

add :: (MonadIO m) => Text -> Text -> ReaderT SqlBackend m (Key Document)
add name text = P.insert (Document name text)

-------------------------------------------------------------------------------
-- * Retrieval

{-|
Retrieves 'Document's with given ID from database.

ASC sorted by name.
-}
get :: (MonadIO m)
    => Key Document
    -> ReaderT SqlBackend m (Maybe Document)
get = P.get

{-|
Retrieves all 'Document's from the database.

ASC sorted by name.
-}
getAll :: (MonadIO m) => ReaderT SqlBackend m [Entity Document]
getAll = P.selectList [] [P.Asc DocumentName]
