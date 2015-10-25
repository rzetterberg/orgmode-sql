{-|
CRUD functionality for 'Document's.
-}
module Database.OrgMode.Model.Clock where

import           Database.Persist

import           Database.OrgMode.Import
import           Database.OrgMode.Model

-------------------------------------------------------------------------------
-- * Creation

{-|
Adds the given 'Document' into the database. Simply a wrapper for persists'
'insert' function.
-}
add :: (MonadIO m) => Clock -> ReaderT SqlBackend m (Key Clock)
add = insert

-------------------------------------------------------------------------------
-- * Retrieval

{-|
Retrieves all 'Document's in the database.

No sorting
-}
getAll :: (MonadIO m) => ReaderT SqlBackend m [Entity Clock]
getAll = selectList [] []

{-|
Retrieves all clocks in the database and sums up all durations to produce the
total amount in seconds.

Input query determines the sorting.
-}
getTotalDuration :: (MonadIO m)
                 => ReaderT SqlBackend m [Entity Clock] -- ^ Query to run
                 -> ReaderT SqlBackend m Int            -- ^ Sum of duration
getTotalDuration = liftM (foldl sumDur 0)
  where
    sumDur curr (Entity _ Clock{..}) = curr + clockDuration
