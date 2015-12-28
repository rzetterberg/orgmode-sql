-- | Special retrieval queries to build 'ClockTable's using 'HeadingFilter's.
--
-- 'ClockTable' is not a type saved directly in the database, it's a combination
-- type used for presenting org-mode data.
-- Should work similarly to
-- <http://orgmode.org/manual/The-clock-table.html The clock table in emacs>.
--
-- = Usage
--
-- To retrieve a 'ClockTable' you only need to use the 'getTable' function and
-- understand how 'HeadingFilter' (see docs for type for usage) works.
--
-- For example if you want to retrieve a 'ClockTable' with from all 'Heading's
-- that have clocks in all 'Document's:
--
-- > import           Data.Default
-- > import qualified Database.OrgMode.Query.ClockTable as ClockTable
-- >
-- > ctable <- ClockTable.getTable def
--
-- If you want to retrieve a 'ClockTable' with all 'Heading's from a specific
-- document you use:
--
-- > ctable <- ClockTable.getTable def { headingFilterDocumentIds = [yourDocId] }

module Database.OrgMode.Export.ClockTable where

import           Control.Monad (unless)
import           Data.Int (Int64)
import           Database.Esqueleto
import qualified Data.HashMap.Strict as HM
import           Data.HashMap.Strict (HashMap)

import           Database.OrgMode.Internal.Import
import           Database.OrgMode.Internal.Types
import           Database.OrgMode.Types

-------------------------------------------------------------------------------
-- * Retrieval

{-|
Retrives a clocktable using the given 'HeadingFilter' for all items.

See the docs of 'HeadingFilter' for more information about how the filter works.

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
getRows hedFilter = shortsToRows `liftM` getShorts hedFilter

{-|
Retrieves a list of 'HeadingShort' using the given 'HeadingFilter'. The list of
'HeadingShort's have the right hierarchy, and is not just a flat list.
The hierarchy is built using the 'mkShortsHierarchy' function.

NB: 'Heading's without clocks are ignored.
-}
getShorts :: (MonadIO m)
          => HeadingFilter
          -> ReaderT SqlBackend m [HeadingShort]
getShorts hedFilter = (mkShortsHierarchy . map unWrap) `liftM` select q
  where
    (HeadingFilter startM endM docIds) = hedFilter
    q = from $ \(doc, heading, clock) -> do
        let docId    = doc ^. DocumentId
            hedId    = heading ^. HeadingId
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

        return $ (heading, docId, hedDurM)
    unWrap :: (Entity Heading, Value (Key Document), Value (Maybe Int))
           -> HeadingShort
    unWrap ((Entity headingId Heading{..}), docId, hedDurM) =
        let docKey    = fromSqlKey (unValue docId)
            hedKey    = fromSqlKey headingId
            hedParKey = fromSqlKey <$> headingParent
            dur       = maybe 0 id (unValue hedDurM)
        in  HeadingShort docKey hedKey hedParKey headingTitle dur []

-------------------------------------------------------------------------------
-- * Helpers

{-|
Helper for running a Monadic function on the given value when it is 'Just'.

Instead of writing:

> case valM of
>   Just val -> myFunc val
>   Nothing  -> return ()

You can write:

> whenJust valM myFunc

And get the same result with less code.
-}
whenJust :: (Monad m)
         => Maybe a     -- ^ Value to use
         -> (a -> m ()) -- ^ Function to apply on pure value
         -> m ()
whenJust (Just v) f = f v
whenJust Nothing _  = return ()

{-|
Takes a flat list of 'HeadingShort's and creates a 'HeadingShort' hierarchy.

NB: This function is really naive and inefficient. It probably has time
complexity of O(n^∞) and you need a quantum computer to run it.
-}
mkShortsHierarchy :: [HeadingShort] -> [HeadingShort]
mkShortsHierarchy inp = map findChildren $ filter isRoot inp
  where
    isRoot :: HeadingShort -> Bool
    isRoot HeadingShort{..} = case headingShortParId of
        Nothing -> True
        _       -> False
    isChild :: Int64 -> HeadingShort -> Bool
    isChild currParent HeadingShort{..}
        = maybe False (currParent ==) headingShortParId
    findChildren curr@HeadingShort{..} =
        let subs = map findChildren $ filter (isChild headingShortId) inp
        in  curr{ headingShortSubs = subs }


{-|
Creates a list of 'ClockRow's from a list of 'HeadingShort'. 'ClockRow's are
matched and created from each 'HeadingShort's document ID.

NB: This function needs to be run _after_ the 'HeadingShort' hierarchy is
created. It should not be run on a flat list.
-}
shortsToRows :: [HeadingShort] -> [ClockRow]
shortsToRows = go emptyMap
  where
    emptyMap :: HashMap Int64 [HeadingShort]
    emptyMap = HM.empty
    go m []     = map (\(k, v) -> ClockRow k 0 v) (HM.toList m)
    go m (h:hs) =
        let newM = HM.insertWith (++) (headingShortDocId h) [h] m
        in  go newM hs
