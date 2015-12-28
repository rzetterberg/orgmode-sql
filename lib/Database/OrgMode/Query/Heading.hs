{-|
CRUD functionality for 'Heading's.
-}
module Database.OrgMode.Query.Heading where

import           Database.Esqueleto
import qualified Database.Persist as P

import           Database.OrgMode.Internal.Import
import           Database.OrgMode.Types
import qualified Database.OrgMode.Query.Clock as ClockQ
import qualified Database.OrgMode.Query.Planning as PlanningQ
import qualified Database.OrgMode.Query.Property as PropertyQ
import qualified Database.OrgMode.Query.TagRel as TagRelQ

-------------------------------------------------------------------------------
-- * Creation

{-|
Inserts the given 'Heading' into the database and returns the given ID.
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

{-|
Retrives all 'Heading's by 'Tag' name

ASC sorted by heading title
-}
getByTag :: (MonadIO m) => Text -> ReaderT SqlBackend m [Entity Heading]
getByTag tagName =
    select $
        from $ \(heading, rel, tag) -> do
            where_ (tag     ^. TagName    ==. val tagName)
            where_ (rel     ^. TagRelItem ==. tag ^. TagId)
            where_ (heading ^. HeadingId  ==. rel ^. TagRelHeading)

            orderBy [asc (heading ^. HeadingTitle)]

            return heading

{-|
Retrives all 'Heading's by 'Document' ID

ASC sorted by heading level for easier tree construction.
-}
getByDocument :: (MonadIO m) => Key Document -> ReaderT SqlBackend m [Entity Heading]
getByDocument docId = P.selectList [HeadingDocument P.==. docId] [P.Asc HeadingLevel]

{-|
Retrives all 'Heading's by list of 'Document' IDs
-}
getByDocuments :: (MonadIO m) => [Key Document] -> ReaderT SqlBackend m [Entity Heading]
getByDocuments docIds =
    select $
        from $ \(heading) -> do
            where_ $ in_ (heading ^. HeadingDocument) (valList docIds)

            return heading

-------------------------------------------------------------------------------
-- * Deletion

{-|
Deletes all 'Heading's and their meta data by given list of IDs.
-}
deleteByIds :: (MonadIO m) => [Key Heading] -> ReaderT SqlBackend m ()
deleteByIds hedIds = do
    ClockQ.deleteByHeadings hedIds
    PlanningQ.deleteByHeadings hedIds
    PropertyQ.deleteByHeadings hedIds
    TagRelQ.deleteByHeadings hedIds

    delete $
        from $ \(heading) -> do
            where_ $ in_ (heading ^. HeadingId) (valList hedIds)

            return ()

{-|
Deletes all 'Heading's and their meta data by given list of 'Document' IDs.
-}
deleteByDocuments :: (MonadIO m) => [Key Document] -> ReaderT SqlBackend m ()
deleteByDocuments docIds = getByDocuments docIds >>= deleteByIds . map extractIds
  where
    extractIds (Entity hedId _) = hedId
