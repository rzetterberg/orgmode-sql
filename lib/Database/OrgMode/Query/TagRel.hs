{-|
CRUD functionality for 'TagRel's.
-}
module Database.OrgMode.Query.TagRel where

import           Database.Esqueleto
import qualified Database.Persist as P

import           Database.OrgMode.Internal.Import
import           Database.OrgMode.Types

-------------------------------------------------------------------------------
-- * Creation

{-|
Creates a 'TagRel' in the database from given data. This represents ownership of
a 'Tag' for a 'Heading'. See more in "OrgMode.Database.Types" for details on
ownership.
-}
add :: (MonadIO m)
    => Key Heading                       -- ^ Heading that has the tag
    -> Key Tag                           -- ^ Tag to use
    -> ReaderT SqlBackend m (Key TagRel) -- ^ ID of created 'TagRel'
add owner tag = P.insert (TagRel owner tag)

-------------------------------------------------------------------------------
-- * Deletion

{-|
Deletes 'TagRel's by list of IDs
-}
deleteByIds :: (MonadIO m)
            => [Key TagRel]
            -> ReaderT SqlBackend m ()
deleteByIds relIds =
    delete $
        from $ \(rel) -> do
            where_ $ in_ (rel ^. TagRelId) (valList relIds)

            return ()

{-|
Deletes all 'TagRel's by given list of 'Heading' IDs.
-}
deleteByHeadings :: (MonadIO m) => [Key Heading] -> ReaderT SqlBackend m ()
deleteByHeadings hedIds =
    delete $
        from $ \(rel) -> do
            where_ $ in_ (rel ^. TagRelHeading) (valList hedIds)

            return ()
