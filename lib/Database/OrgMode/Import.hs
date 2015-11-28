{-|
-}

module Database.OrgMode.Import where

import           Data.OrgMode.Parse.Types
import           Data.Text (strip)
import           Data.Time.Clock (UTCTime(..))
import qualified Data.HashMap.Strict as HM
import qualified Data.Time.Calendar as T
import qualified Data.Time.Clock as T

import           Database.OrgMode.Internal.Import
import qualified Database.OrgMode.Query.Clock as DbClock
import qualified Database.OrgMode.Query.Document as DbDocument
import qualified Database.OrgMode.Query.Heading as DbHeading
import qualified Database.OrgMode.Query.Planning as DbPlanning
import qualified Database.OrgMode.Query.Property as DbProperty
import qualified Database.OrgMode.Query.Tag as DbTag
import qualified Database.OrgMode.Query.TagRel as DbTagRel
import qualified Database.OrgMode.Types as Db

{-|
Takes a parsed document and it's name and inserts it into the database. The name
could be anything, usually the name of the orgmode file is used.

Takes care of importing the whole chain of data (headings, clocks, tags, etc).
Think of this as an all inclusive holiday into database land.

The ID of the document in the database is returned.
-}
importDocument :: (MonadIO m)
               => Text                                   -- ^ Document name
               -> Document                               -- ^ Parsed document
               -> ReaderT SqlBackend m (Key Db.Document) -- ^ ID of created doc
importDocument docName Document{..} = do
    docId <- DbDocument.add docName (strip documentText)

    mapM_ (importHeading docId Nothing) documentHeadings

    return docId

{-|
Takes a parsed heading and inserts it into the database. Since headings in
org-mode have a tree structure each 'Heading' can contain an ID to it's parent.

Headings belong to a specific document that's why the ID is needed when
inserting a 'Heading' into the database.

NB: this function recurses on each 'subHeadings' found.
-}
importHeading :: (MonadIO m)
              => Key Db.Document                       -- ^ ID of document owner
              -> Maybe (Key Db.Heading)                -- ^ ID of parent heading
              -> Heading                               -- ^ Parsed heading
              -> ReaderT SqlBackend m (Key Db.Heading) -- ^ ID of the heading
importHeading docId parentM Heading{..} = do
    headingId <- DbHeading.add dbHeading

    mapM_ (importTag headingId) tags
    mapM_ (importClock headingId) sectionClocks
    mapM_ (importPlanning headingId) (HM.toList pmap)
    mapM_ (importProperty headingId) (HM.toList sectionProperties)
    mapM_ (importHeading docId (Just headingId)) subHeadings

    return headingId
  where
    Section{..} = section
    (Plns pmap) = sectionPlannings
    (Level lvl) = level
    kwordM      = fmap unStateKeyword keyword
    dbHeading   = Db.Heading { headingDocument  = docId
                             , headingParent    = parentM
                             , headingLevel     = lvl
                             , headingKeyword   = kwordM
                             , headingPriority  = priority
                             , headingTitle     = title
                             , headingParagraph = strip sectionParagraph
                             }

{-|
Imports the given tag into the database and returns the ID it was given.

NB: This function also sets up the relation between the heading it belongs to.
-}
importTag :: (MonadIO m)
          => Key Db.Heading                    -- ^ ID of owner
          -> Tag                               -- ^ Parsed tag name
          -> ReaderT SqlBackend m (Key Db.Tag) -- ^ Given ID
importTag headingId tagName = do
    tagId <- DbTag.add tagName

    void $ DbTagRel.add headingId tagId

    return tagId

{-|
Imports the given clock into the database and returns the ID it was given.

NB: This function also handles the relationship between a heading and the
given clock.

Also, a tuple is used since orgmode-parse does not expose the Clock type.
-}
importClock :: (MonadIO m)
            => Key Db.Heading                              -- ^ ID of owner
            -> (Maybe Timestamp, Maybe Duration)           -- ^ Clock to insert
            -> ReaderT SqlBackend m (Maybe (Key Db.Clock)) -- ^ Given ID
importClock _         (Nothing, _)     = return Nothing
importClock headingId (Just tstamp, _)
    = DbClock.add headingId tsActive start endM dur >>= return . Just
  where
    Timestamp{..} = tstamp
    start = dateTimeToUTC tsTime
    endM  = dateTimeToUTC <$> tsEndTime
    dur   = utcToDur start endM

{-|
Imports given planning into the database and returns the ID it was given.

NB: A tuple is used since 'Plannings' is a 'HashMap' with 'PlanningKeyword'
as key and 'Timestamp' as value. There is not a type alias for a single Planning
in orgmode-parse.
-}
importPlanning :: (MonadIO m)
               => Key Db.Heading                         -- ^ ID of owner
               -> (PlanningKeyword, Timestamp)           -- ^ Planning to insert
               -> ReaderT SqlBackend m (Key Db.Planning) -- ^ Given ID
importPlanning hedId (kword, tstamp)
    = DbPlanning.add hedId kword start
  where
    Timestamp{..} = tstamp
    start = dateTimeToUTC tsTime

{-|
Imports given property into the database and returns the ID it was given.

NB: A tuple is used since 'Properties' is a 'HashMap' with 'Text'
as key and 'Text' as value. There is not a type alias for a single Property
in orgmode-parse.
-}
importProperty :: (MonadIO m)
               => Key Db.Heading                         -- ^ ID of owner
               -> (Text, Text)                           -- ^ Property to insert
               -> ReaderT SqlBackend m (Key Db.Property) -- ^ Given ID
importProperty headingId (key, val) = DbProperty.add headingId key val

-------------------------------------------------------------------------------
-- ** Conversion

{-|
Helper for converting a orgmode-parse 'DateTime' into a 'UTCTime'. 'UTCTime' is
used for convenience when calculating the duration of two 'DateTime's.
-}
dateTimeToUTC :: DateTime
              -> UTCTime
dateTimeToUTC (DateTime cal _ clock _ _)
    = UTCTime (T.fromGregorian (toInteger year) month day)
              (T.secondsToDiffTime midSecs)
  where
    (YMD' (YearMonthDay year month day)) = cal
    calcSecs Nothing       = 0
    calcSecs (Just (h, m)) = ((h * 60) + m) * 60
    midSecs                = toInteger $ calcSecs clock

{-|
Helper for calculating the duration between two 'UTCTime'. Since 'Timestamp's can
can have the end time missing, this function accepts 'Maybe UTCTime'. When the
end time is missing a duration of 0 is returned.

NB: duration is returned as seconds.

20 seconds between start and end:

>>> utcToDur start (Just end)
20

No end time given:

>>> utcToDur start Nothing
0
-}
utcToDur :: UTCTime -> Maybe UTCTime -> Int
utcToDur start endM = case endM of
    Nothing  -> 0
    Just end -> round $ T.diffUTCTime end start
