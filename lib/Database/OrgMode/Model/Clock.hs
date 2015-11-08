{-|
CRUD functionality for 'Document's.
-}
module Database.OrgMode.Model.Clock where

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
add :: (MonadIO m) => Clock -> ReaderT SqlBackend m (Key Clock)
add = P.insert

-------------------------------------------------------------------------------
-- * Retrieval

{-|
Retrieves all 'Document's in the database.

ASC sorted by clock start.
-}
getAll :: (MonadIO m) => ReaderT SqlBackend m [Entity Clock]
getAll = P.selectList [] [P.Asc ClockStart]

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

{-|
Retrives all 'Clock's by 'Heading' Id.

ASC sorted by clock start.
-}
getByHeading :: (MonadIO m) => Key Heading -> ReaderT SqlBackend m [Entity Clock]
getByHeading headingId =
    select $
        from $ \(heading, clock) -> do
            where_ (heading ^. HeadingId  ==. val headingId)
            where_ (clock   ^. ClockOwner ==. heading ^. HeadingSection)

            orderBy [asc (clock ^. ClockStart)]

            return clock

{-|
Retrives all 'Clock's by 'Tag' name

ASC sorted by clock start.
-}
getByTag :: (MonadIO m) => Text -> ReaderT SqlBackend m [Entity Clock]
getByTag tagName =
    select $
        from $ \(clock, heading, rel, tag) -> do
            where_ (tag     ^. TagName    ==. val tagName)
            where_ (rel     ^. TagRelItem ==. tag     ^. TagId)
            where_ (heading ^. HeadingId  ==. rel     ^. TagRelOwner)
            where_ (clock   ^. ClockOwner ==. heading ^. HeadingSection)

            orderBy [asc (clock ^. ClockStart)]

            return clock

{-|
Retrieves total duration for each 'Tag'

DESC sorted by total duration
-}
getTagTotal :: (MonadIO m) => ReaderT SqlBackend m [(Text, Int)]
getTagTotal = do
    res <- select $
        from $ \(clock, tag, rel, heading) -> do
            let tname = tag ^. TagName
                total = sum_ (clock ^. ClockDuration)

            where_ (rel     ^. TagRelItem ==. tag     ^. TagId)
            where_ (heading ^. HeadingId  ==. rel     ^. TagRelOwner)
            where_ (clock   ^. ClockOwner ==. heading ^. HeadingSection)

            groupBy tname
            orderBy [desc total]

            return (tname, total)

    return $ map prepare res
  where
    prepare (Value n, Value totM) = (n, maybe 0 id totM)

{-|
Retrieves total duration for each 'Heading'

DESC sorted by total duration
-}
getHeadingTotal :: (MonadIO m) => ReaderT SqlBackend m [(Text, Int)]
getHeadingTotal = do
    res <- select $
        from $ \(clock, tag, rel, heading) -> do
            let hname = heading ^. HeadingTitle
                total = sum_ (clock ^. ClockDuration)

            where_ (rel     ^. TagRelItem ==. tag     ^. TagId)
            where_ (heading ^. HeadingId  ==. rel     ^. TagRelOwner)
            where_ (clock   ^. ClockOwner ==. heading ^. HeadingSection)

            groupBy hname
            orderBy [desc total]

            return (hname, total)

    return $ map prepare res
  where
    prepare (Value n, Value totM) = (n, maybe 0 id totM)

{-|
Retrieves total duration for each 'Document'

DESC sorted by total duration
-}
getDocumentTotal :: (MonadIO m) => ReaderT SqlBackend m [(Text, Int)]
getDocumentTotal = do
    res <- select $
        from $ \(doc, clock, tag, rel, heading) -> do
            let dname = doc ^. DocumentName
                total = sum_ (clock ^. ClockDuration)

            where_ (doc     ^. DocumentId ==. heading ^. HeadingDocument)
            where_ (heading ^. HeadingId  ==. rel     ^. TagRelOwner)
            where_ (rel     ^. TagRelItem ==. tag     ^. TagId)
            where_ (clock   ^. ClockOwner ==. heading ^. HeadingSection)

            groupBy dname
            orderBy [desc total]

            return (dname, total)

    return $ map prepare res
  where
    prepare (Value n, Value totM) = (n, maybe 0 id totM)
