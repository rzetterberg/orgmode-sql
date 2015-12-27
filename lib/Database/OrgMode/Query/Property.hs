{-|
CRUD functionality for 'Property's.
-}
module Database.OrgMode.Query.Property where

import           Database.Esqueleto
import qualified Database.Persist as P

import           Database.OrgMode.Internal.Import
import           Database.OrgMode.Types

-------------------------------------------------------------------------------
-- * Creation

add :: (MonadIO m)
    => Key Heading
    -> Text
    -> Text
    -> ReaderT SqlBackend m (Key Property)
add owner key value = P.insert (Property owner key value)

-------------------------------------------------------------------------------
-- * Retrieval

{-|
Retrives all 'Property's by 'Heading' Id.

ASC sorted by clock start.
-}
getByHeading :: (MonadIO m)
             => Key Heading
             -> ReaderT SqlBackend m [Entity Property]
getByHeading hedId =
    select $
        from $ \(heading, property) -> do
            where_ (heading ^. HeadingId  ==. val hedId)
            where_ (property ^. PropertyHeading ==. heading ^. HeadingId)

            return property

-------------------------------------------------------------------------------
-- * Deletion

{-|
Deletes all 'Properties's by given list of 'Heading' IDs.
-}
deleteByHeadings :: (MonadIO m) => [Key Heading] -> ReaderT SqlBackend m ()
deleteByHeadings hedIds =
    delete $
        from $ \(property) -> do
            where_ $ in_ (property ^. PropertyHeading) (valList hedIds)

            return ()
