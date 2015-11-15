{-|
CRUD functionality for 'Planning's.
-}
module Database.OrgMode.Query.Planning where

import           Data.OrgMode.Parse.Types (PlanningKeyword(..))
import           Database.Esqueleto
import qualified Database.Persist as P

import           Database.OrgMode.Import
import           Database.OrgMode.Model

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
