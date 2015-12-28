{-|
CRUD functionality for 'Tag's.
-}
module Database.OrgMode.Internal.Query.Tag where

import           Database.Esqueleto
import qualified Database.Persist as P

import           Database.OrgMode.Internal.Import
import           Database.OrgMode.Internal.Types

-------------------------------------------------------------------------------
-- * Creation

{-|
Adds the given tag into the database. If a tag with the same unique name exists
no tag is added and the ID of the current one is returned instead.
-}
add :: (MonadIO m)
    => Text                           -- ^ Tag name
    -> ReaderT SqlBackend m (Key Tag) -- ^ Current ID or ID of newly created tag
add name = P.getBy (UniqueTag name) >>= getCurrId
  where
    getCurrId (Just (Entity currId _ )) = return currId
    getCurrId Nothing                   = P.insert (Tag name)

-------------------------------------------------------------------------------
-- * Retrieval

{-|
Retrieves all 'Tag's found in the database.

ASC sorted by name.
-}
getAll :: (MonadIO m) => ReaderT SqlBackend m [Entity Tag]
getAll = P.selectList [] [P.Asc TagName]

{-|
Retrives all 'Tag's by a 'Document' name.

ASC sorted by name.
-}
getByDocumentName :: (MonadIO m) => Text -> ReaderT SqlBackend m [Entity Tag]
getByDocumentName docName =
    select $
        from $ \(doc, heading, rel, tag) -> do
            where_ (doc     ^. DocumentName ==. val docName)
            where_ (doc     ^. DocumentId   ==. heading ^. HeadingDocument)
            where_ (heading ^. HeadingId    ==. rel     ^. TagRelHeading)
            where_ (rel     ^. TagRelItem   ==. tag     ^. TagId)

            orderBy [asc (tag ^. TagName)]

            return tag

{-|
Retrives all 'Tag's by 'Heading' Id.

ASC sorted by name.
-}
getByHeading :: (MonadIO m) => Key Heading -> ReaderT SqlBackend m [Entity Tag]
getByHeading headingId =
    select $
        from $ \(tag, rel) -> do
            where_ (rel ^. TagRelHeading ==. val headingId)
            where_ (rel ^. TagRelItem    ==. tag ^. TagId)

            orderBy [asc (tag ^. TagName)]

            return tag

-------------------------------------------------------------------------------
-- * Deletion

{-|
Deletes 'Tag's by list of IDs
-}
deleteByIds :: (MonadIO m)
            => [Key Tag]
            -> ReaderT SqlBackend m ()
deleteByIds tagIds =
    delete $
        from $ \(tag) -> do
            where_ $ in_ (tag ^. TagId) (valList tagIds)

            return ()
