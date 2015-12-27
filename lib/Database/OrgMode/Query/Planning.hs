{-|
CRUD functionality for 'Planning's.
-}
module Database.OrgMode.Query.Planning where

import           Data.OrgMode.Parse.Types (PlanningKeyword(..))
import           Database.Esqueleto
import qualified Database.Persist as P

import           Database.OrgMode.Internal.Import
import           Database.OrgMode.Types

-------------------------------------------------------------------------------
-- * Creation

add :: (MonadIO m)
    => Key Heading
    -> PlanningKeyword
    -> UTCTime
    -> ReaderT SqlBackend m (Key Planning)
add owner keyword time = P.insert (Planning owner keyword time)

-------------------------------------------------------------------------------
-- * Retrieval

{-|
Retrives all 'Planning's by 'Heading' Id.

ASC sorted by clock start.
-}
getByHeading :: (MonadIO m)
             => Key Heading
             -> ReaderT SqlBackend m [Entity Planning]
getByHeading hedId =
    select $
        from $ \(heading, planning) -> do
            where_ (heading ^. HeadingId  ==. val hedId)
            where_ (planning ^. PlanningHeading ==. heading ^. HeadingId)

            return planning

-------------------------------------------------------------------------------
-- * Deletion

{-|
Deletes all 'Planning's by given list of 'Heading' IDs.
-}
deleteByHeadings :: (MonadIO m) => [Key Heading] -> ReaderT SqlBackend m ()
deleteByHeadings hedIds =
    delete $
        from $ \(planning) -> do
            where_ $ in_ (planning ^. PlanningHeading) (valList hedIds)

            return ()
