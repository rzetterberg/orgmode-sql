{-|
CRUD functionality for 'Clocks's.
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
Retrives all 'Clock's by 'Tag' name.

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
Retrieves total duration for each 'Tag'. Groups by name of the 'Tag'.
Outputs 'Tag's without clocks as just 0 total.

DESC sorted by total duration
-}
getTagTotal :: (MonadIO m) => ReaderT SqlBackend m [(Text, Int)]
getTagTotal = do
    res <- select $
        from $ \(tag `LeftOuterJoin` rel `LeftOuterJoin` heading `LeftOuterJoin` clock) -> do
            let tagName = tag ^. TagName
                total   = sum_ (clock ?. ClockDuration)

            on (tag   ^. TagId       ==. rel ^. TagRelItem)
            on (rel   ^. TagRelOwner ==. heading ^. HeadingId)
            on (clock ?. ClockOwner  ==. just (heading ^. HeadingSection))

            groupBy tagName
            orderBy [desc total]

            return (tagName, total)

    return $ map prepare res
  where
    prepare (Value tagName, Value totM) = (tagName, maybe 0 id totM)

{-|
Retrieves total duration for each 'Heading'. Groups by title of the 'Heading'.
Outputs 'Heading's without clocks as just 0 total.

DESC sorted by total duration
-}
getHeadingTotal :: (MonadIO m) => ReaderT SqlBackend m [(Key Heading, Text, Int)]
getHeadingTotal = do
    res <- select $
        from $ \(heading `LeftOuterJoin` clock) -> do
            let headId    = heading ^. HeadingId
                headTitle = heading ^. HeadingTitle
                total     = sum_ (clock ?. ClockDuration)

            on (clock ?. ClockOwner ==. just (heading ^. HeadingSection))

            groupBy headId
            orderBy [desc total]

            return (headId, headTitle, total)

    return $ map prepare res
  where
    prepare (Value headId, Value headName, Value totM)
        = (headId, headName, maybe 0 id totM)

{-|
Retrieves total duration for each 'Document'. Groups by name of the 'Document'.
Outputs 'Document's without clocks as just 0 total.

DESC sorted by total duration
-}
getDocumentTotal :: (MonadIO m) => ReaderT SqlBackend m [(Key Document, Text, Int)]
getDocumentTotal = do
    res <- select $
        from $ \(doc `LeftOuterJoin` heading `LeftOuterJoin` clock) -> do
            let docId   = doc ^. DocumentId
                docName = doc ^. DocumentName
                total   = sum_ (clock ?. ClockDuration)

            on (docId               ==. heading       ^. HeadingDocument)
            on (clock ?. ClockOwner ==. just (heading ^. HeadingSection))

            groupBy docId
            orderBy [desc total]

            return (docId, docName, total)

    return $ map prepare res
  where
    prepare (Value docId, Value docName, Value totM)
        = (docId, docName, maybe 0 id totM)
