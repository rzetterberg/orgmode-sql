{-|
CRUD functionality for 'Clock's.
-}
module Database.OrgMode.Internal.Query.Clock where

import           Database.Esqueleto
import qualified Database.Persist as P

import           Database.OrgMode.Internal.Import
import           Database.OrgMode.Internal.Types
import           Database.OrgMode.Internal.Convert.Time (utcToDuration)

-------------------------------------------------------------------------------
-- * Creation

{-|
Inserts the given 'Clock' data into the database.
-}
add :: (MonadIO m)
    => Key Heading                      -- ^ Heading the clock belongs to
    -> Bool                             -- ^ Is clock active?
    -> UTCTime                          -- ^ Start time of clock
    -> Maybe UTCTime                    -- ^ End time of clock
    -> ReaderT SqlBackend m (Key Clock) -- ^ ID of created clock
add owner active start endM = P.insert $
    Clock owner active start endM (utcToDuration start endM)

-------------------------------------------------------------------------------
-- * Retrieval

{-|
Retrieves all 'Clock's in the database.

ASC sorted by clock start.
-}
getAll :: (MonadIO m)
       => ReaderT SqlBackend m [Entity Clock]
getAll = P.selectList [] []

{-|
Retrieves all 'Clock's in the database and sums up all durations to produce the
total amount in seconds.

Input query determines the sorting.
-}
getTotalDuration :: (MonadIO m)
                 => ReaderT SqlBackend m Int
getTotalDuration = do
    tstamps <- P.selectList [] []

    return (foldl sumDur 0 tstamps)
  where
    sumDur curr (Entity _ Clock{..}) = curr + clockDuration

{-|
Retrives all 'Clock's by 'Heading' Id.

ASC sorted by clock start.
-}
getByHeading :: (MonadIO m)
             => Key Heading
             -> ReaderT SqlBackend m [Entity Clock]
getByHeading headingId =
    select $
        from $ \(heading, clock) -> do
            where_ (heading ^. HeadingId  ==. val headingId)
            where_ (clock ^. ClockHeading ==. heading ^. HeadingId)

            return clock

{-|
Retrives all 'Clock's by 'Tag' name.

ASC sorted by clock start.
-}
getByTag :: (MonadIO m) => Text -> ReaderT SqlBackend m [Entity Clock]
getByTag tagName =
    select $
        from $ \(clock, heading, rel, tag) -> do
            where_ (tag     ^. TagName      ==. val tagName)
            where_ (rel     ^. TagRelItem   ==. tag     ^. TagId)
            where_ (heading ^. HeadingId    ==. rel     ^. TagRelHeading)
            where_ (clock   ^. ClockHeading ==. heading ^. HeadingId)

            return clock

-------------------------------------------------------------------------------
-- * Deletion

{-|
Deletes all 'Clock's in the database.
-}
deleteAll :: (MonadIO m)
          => ReaderT SqlBackend m ()
deleteAll = P.deleteWhere ([] :: [P.Filter Clock])

{-|
Deletes 'Clock's by list of IDs
-}
deleteByIds :: (MonadIO m)
            => [Key Clock]
            -> ReaderT SqlBackend m ()
deleteByIds clockIds =
    delete $
        from $ \(clock) -> do
            where_ $ in_ (clock ^. ClockId) (valList clockIds)

            return ()

{-|
Deletes all 'Clock's by given list of 'Heading' IDs.
-}
deleteByHeadings :: (MonadIO m) => [Key Heading] -> ReaderT SqlBackend m ()
deleteByHeadings hedIds =
    delete $
        from $ \(clock) -> do
            where_ $ in_ (clock ^. ClockHeading) (valList hedIds)

            return ()
