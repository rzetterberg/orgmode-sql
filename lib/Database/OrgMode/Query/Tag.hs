{-|
CRUD functionality for 'Tag's.
-}
module Database.OrgMode.Query.Tag where

import           Database.Esqueleto
import qualified Database.Persist as P

import           Database.OrgMode.Import
import           Database.OrgMode.Model

-------------------------------------------------------------------------------
-- * Creation

{-|
Adds the given tag name into the database and assigns the 'Heading' with
the given ID the owner of the tag.

Note that tag names are unique in the database so if the tag name already
exists that tag name will be used when creating the relationship between
the 'Heading' and the 'Tag'.
-}
add :: (MonadIO m) => Key Heading -> Text -> ReaderT SqlBackend m (Key Tag)
add headingId tagName = do
    currId <- getBy (UniqueTag tagName) >>= getCurrId

    void $ P.insert (TagRel headingId currId)

    return currId
  where
    getCurrId (Just (Entity currId _ )) = return currId
    getCurrId Nothing                   = P.insert (Tag tagName)

-------------------------------------------------------------------------------
-- * Retrieval

{-|
Retrieves all 'Tag's found in the database.

ASC sorted by name.
-}
getAll :: (MonadIO m) => ReaderT SqlBackend m [Entity Tag]
getAll = P.selectList [] [P.Asc TagName]

{-|
Retrives all 'Tag's by a 'Document' name.

ASC sorted by name.
-}
getByDocumentName :: (MonadIO m) => Text -> ReaderT SqlBackend m [Entity Tag]
getByDocumentName docName =
    select $
        from $ \(doc, heading, rel, tag) -> do
            where_ (doc     ^. DocumentName ==. val docName)
            where_ (doc     ^. DocumentId   ==. heading ^. HeadingDocument)
            where_ (heading ^. HeadingId    ==. rel     ^. TagRelOwner)
            where_ (rel     ^. TagRelItem   ==. tag     ^. TagId)

            orderBy [asc (tag ^. TagName)]

            return tag

{-|
Retrives all 'Tag's by 'Heading' Id.

ASC sorted by name.
-}
getByHeading :: (MonadIO m) => Key Heading -> ReaderT SqlBackend m [Entity Tag]
getByHeading headingId =
    select $
        from $ \(tag, rel) -> do
            where_ (rel ^. TagRelOwner ==. val headingId)
            where_ (rel ^. TagRelItem  ==. tag ^. TagId)

            orderBy [asc (tag ^. TagName)]

            return tag

{-|
Retrieves total duration for each 'Tag'. Groups by name of the 'Tag'.
Outputs 'Tag's without clocks as just 0 total.

DESC sorted by total duration
-}
getTotals :: (MonadIO m) => ReaderT SqlBackend m [TagTotal]
getTotals = do
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
    prepare (Value tagName, Value totM) = TagTotal tagName (maybe 0 id totM)
