{-|
CRUD functionality for 'ClockTable's.
-}
module Database.OrgMode.Query.ClockTable where

import           Control.Monad (unless)
import           Data.Int (Int64)
import           Database.Esqueleto
import qualified Data.HashMap.Strict as HM
import           Data.HashMap.Strict (HashMap)

import           Database.OrgMode.Internal.Import
import           Database.OrgMode.Types

-------------------------------------------------------------------------------
-- * Retrieval

{-|
Retrives a clocktable using the given 'HeadingFilter' for all items.

NB: Does not check that the given date range is valid.
-}
getTable :: (MonadIO m)
         => HeadingFilter
         -> ReaderT SqlBackend m ClockTable
getTable hedFilter = do
    rows <- getRows hedFilter

    return $ ClockTable rows hedFilter

{-|
Retrieves all 'ClockRow's from the database using the given 'HeadingFilter'
for all items.
-}
getRows :: (MonadIO m)
        => HeadingFilter
        -> ReaderT SqlBackend m [ClockRow]
getRows hedFilter = shortsToRows `liftM` getShortParts hedFilter
  where
    shortsToRows = HM.foldrWithKey foldRow [] . mapParts emptyMap
    foldRow :: Int64 -> [HeadingShort] -> [ClockRow] -> [ClockRow]
    foldRow k v res = (ClockRow k 0 v) : res
    emptyMap :: HashMap Int64 [HeadingShort]
    emptyMap = HM.empty
    mapParts m []                = m
    mapParts m ((k, v):rest) =
        let mNew = HM.insertWith (++) k [v] m
        in mapParts mNew rest

{-|
Retrieves all 'HeadingShort's in the database and pairs them up with the
document they belong to by ID.

NB: Shorts will only be retrieved if they have clocks saved.
Ordered by ID.
-}
getShortParts :: (MonadIO m)
              => HeadingFilter
              -> ReaderT SqlBackend m [(Int64, HeadingShort)]
getShortParts hedFilter = map unWrap `liftM` select q
  where
    (HeadingFilter startM endM docIds) = hedFilter
    q = from $ \(doc, heading, clock) -> do
        let docId    = doc ^. DocumentId
            hedId    = heading ^. HeadingId
            hedTitle = heading ^. HeadingTitle
            hedDurM  = sum_ (clock ^. ClockDuration)

        where_ $ heading ^. HeadingDocument ==. docId
        where_ $ clock   ^. ClockHeading    ==. hedId

        whenJust startM $
            \startT -> where_ (clock ^. ClockStart >=. val startT)

        whenJust endM $
            \endT -> where_ (clock ^. ClockEnd <=. just (val endT))

        unless (null docIds) $
            where_ (in_ docId (valList docIds))

        groupBy hedId

        return $ (docId, hedId, hedTitle, hedDurM)
    unWrap (docId, hedId, hedTitle, hedDurM) =
        let docKey = fromSqlKey (unValue docId)
            hedKey = fromSqlKey (unValue hedId)
            dur    = maybe 0 id (unValue hedDurM)
            short  = HeadingShort hedKey (unValue hedTitle) dur
        in (docKey, short)

whenJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
whenJust (Just v) f = f v
whenJust Nothing _  = return ()
