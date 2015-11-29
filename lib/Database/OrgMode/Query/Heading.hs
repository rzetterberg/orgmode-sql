{-|
CRUD functionality for 'Heading's.
-}
module Database.OrgMode.Query.Heading where

import           Database.Esqueleto
import qualified Database.Persist as P

import           Database.OrgMode.Internal.Import
import           Database.OrgMode.Types

-------------------------------------------------------------------------------
-- * Creation

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
