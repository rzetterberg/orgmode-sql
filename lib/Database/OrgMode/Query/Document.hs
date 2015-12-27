{-|
CRUD functionality for 'Document's.
-}
module Database.OrgMode.Query.Document where

import           Database.Esqueleto
import qualified Database.Persist as P

import           Database.OrgMode.Internal.Import
import           Database.OrgMode.Types
import qualified Database.OrgMode.Query.Heading as HeadingQ

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

-------------------------------------------------------------------------------
-- * Deletion

{-|
Deletes all 'Documents's by given list of IDs.

NB: Deletes all 'Heading's of each document too
-}
deleteByIds :: (MonadIO m) => [Key Document] -> ReaderT SqlBackend m ()
deleteByIds docIds = do
    HeadingQ.deleteByDocuments docIds

    delete $
        from $ \(doc) -> do
            where_ $ in_ (doc ^. DocumentId) (valList docIds)

            return ()
