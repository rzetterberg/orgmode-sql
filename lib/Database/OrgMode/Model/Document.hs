{-|
CRUD functionality for 'Document's.
-}
module Database.OrgMode.Model.Document where

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
add :: (MonadIO m) => Document -> ReaderT SqlBackend m (Key Document)
add = P.insert

-------------------------------------------------------------------------------
-- * Retrieval

{-|
Retrieves all 'Document's in the database.

ASC sorted by name.
-}
getAll :: (MonadIO m) => ReaderT SqlBackend m [Entity Document]
getAll = P.selectList [] [P.Asc DocumentName]

{-|
Retrieves total duration for each 'Document'. Groups by name of the 'Document'.
Outputs 'Document's without clocks as just 0 total.

DESC sorted by total duration
-}
getTotals :: (MonadIO m) => ReaderT SqlBackend m [DocumentTotal]
getTotals = do
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
        = DocumentTotal docId docName (maybe 0 id totM)
