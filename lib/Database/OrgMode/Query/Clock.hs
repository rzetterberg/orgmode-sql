{-|
CRUD functionality for 'Clock's.
-}
module Database.OrgMode.Query.Clock where

import           Database.Esqueleto
import qualified Database.Persist as P

import           Database.OrgMode.Import
import           Database.OrgMode.Model

-------------------------------------------------------------------------------
-- * Creation

{-|
Adds the given 'Document' into the database. Simply a wrapper for persists'
'insert' function.
-}
add :: (MonadIO m)
    => Key Heading
    -> Bool
    -> UTCTime
    -> Maybe UTCTime
    -> Int
    -> ReaderT SqlBackend m (Key Clock)
add owner active start endM dur = P.insert $ Clock owner active start endM dur

-------------------------------------------------------------------------------
-- * Retrieval

{-|
Retrieves all 'Document's in the database.

ASC sorted by clock start.
-}
getAll :: (MonadIO m)
       => ReaderT SqlBackend m [Entity Clock]
getAll = P.selectList [] []

{-|
Retrieves all clocks in the database and sums up all durations to produce the
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