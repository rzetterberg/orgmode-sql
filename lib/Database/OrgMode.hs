{-|
A library that parses org-mode documents, imports the data into an SQL database,
provides the user with common queries on the data and functionality to export
the data to different data formats.

The goal of this library is to be usable with MySQL, PostgreSQL and SQLite.
However, currently testing is only performed on SQLite.

The foundation of this library is built using orgmode-parse and persistent.
Those two libraries takes care of the heavy lifting and solves the real
problems.

NB: this library has own data types for org-mode data that have the
same names as the ones in orgmode-parse. The difference is the data types
in this library are simplified and adapted for use in SQL-databases.

The current feature status of this library is:

- Parsing: 100%
- Database creation: 100%
- Common queries: 100%
- Complex queries: 10%
- Exporting: 10%
-}

module Database.OrgMode where

import           Data.Attoparsec.Text (parseOnly)
import           Data.OrgMode.Parse.Types
import qualified Data.OrgMode.Parse.Attoparsec.Document as OrgParse
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.Clock (UTCTime(..), secondsToDiffTime, diffUTCTime)

import           Database.OrgMode.Import
import qualified Database.OrgMode.Model as Db
import qualified Database.OrgMode.Query.Document as DbDocument
import qualified Database.OrgMode.Query.Heading as DbHeading
import qualified Database.OrgMode.Query.Tag as DbTag
import qualified Database.OrgMode.Query.TagRel as DbTagRel
import qualified Database.OrgMode.Query.Clock as DbClock
import qualified Database.OrgMode.Query.DateTime as DbDateTime
import qualified Database.OrgMode.Query.Timestamp as DbTimestamp
import qualified Database.OrgMode.Query.Planning as DbPlanning
import qualified Database.OrgMode.Query.Property as DbProperty

-------------------------------------------------------------------------------
-- * Unparsed raw data import

{-|
Takes a strict 'Text' and tries to parse the document and import it to the
database.

Returns the database ID of the created document or the error message from
the parser.
-}
parseTextImport :: (MonadIO m)
                => Text                    -- ^ Name of the document
                -> [Text]                  -- ^ Keywords to allow
                -> Text                    -- ^ org-mode document contents
                -> ReaderT SqlBackend m (Either String (Key Db.Document))
parseTextImport docName keywords orgContent =
    case result of
        Left  err -> return (Left err)
        Right doc -> Right `liftM` importDocument docName doc
  where
    result = parseOnly (OrgParse.parseDocument keywords) orgContent

-------------------------------------------------------------------------------
-- * Parsed data import

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
    docId <- DbDocument.add docName documentText

    mapM_ (importHeading docId Nothing) documentHeadings

    return docId

{-|
Takes a parsed heading and inserts it into the database. Since headings in
org-mode have a tree structure each 'Heading' can contain an ID to it's parent.

Headings belong to a specific document that's why the ID is needed when
inserting a 'Heading' into the database.

Note: this function recurses on each 'subHeadings' found.
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
                             , headingParagraph = sectionParagraph
                             }

importTag :: (MonadIO m)
          => Key Db.Heading
          -> Tag
          -> ReaderT SqlBackend m (Key Db.Tag)
importTag headingId tagName = do
    tagId <- DbTag.add tagName

    void $ DbTagRel.add headingId tagId

    return tagId

importTimestamp :: (MonadIO m)
                => Timestamp
                -> ReaderT SqlBackend m (Key Db.Timestamp)
importTimestamp tstamp = do
    startId <- DbDateTime.add (dateTimeToDb tsTime)
    endIdM  <- case tsEndTime of
        Nothing  -> return Nothing
        Just end -> do
            endId <- DbDateTime.add (dateTimeToDb end)
            return (Just endId)

    DbTimestamp.add startId tsActive endIdM dur
  where
    Timestamp{..} = tstamp
    dur = dateTimeToDur tsTime tsEndTime

importClock :: (MonadIO m)
            => Key Db.Heading
            -> (Maybe Timestamp, Maybe Duration)
            -> ReaderT SqlBackend m (Maybe (Key Db.Clock))
importClock _         (Nothing, _)     = return Nothing
importClock headingId (Just tstamp, _) = do
    tstampId <- importTimestamp tstamp

    clockId <- DbClock.add headingId tstampId

    return (Just clockId)

importPlanning :: (MonadIO m)
               => Key Db.Heading
               -> (PlanningKeyword, Timestamp)
               -> ReaderT SqlBackend m (Key Db.Planning)
importPlanning headingId (kword, tstamp) = do
    tstampId <- importTimestamp tstamp

    DbPlanning.add headingId kwordT tstampId
  where
    kwordT = T.pack (show kword)

importProperty :: (MonadIO m)
               => Key Db.Heading
               -> (Text, Text)
               -> ReaderT SqlBackend m (Key Db.Property)
importProperty headingId (key, val) = DbProperty.add headingId key val

-------------------------------------------------------------------------------
-- * Data export



-------------------------------------------------------------------------------
-- * Data conversion helpers

dateTimeToDb :: DateTime -> Db.DateTime
dateTimeToDb (DateTime cal _ clockM _ _)
    = Db.DateTime year month day hour minute
  where
    (YMD' (YearMonthDay year month day)) = cal
    (hour, minute) = case clockM of
        Nothing     -> (0, 0)
        Just (h, m) -> (h, m)

dateTimeToUTC :: DateTime -> UTCTime
dateTimeToUTC (DateTime cal _ clock _ _)
    = UTCTime (fromGregorian (toInteger year) month day)
              (secondsToDiffTime midSecs)
  where
    (YMD' (YearMonthDay year month day)) = cal
    calcSecs Nothing       = 0
    calcSecs (Just (h, m)) = ((h * 60) + m) * 60
    midSecs                = toInteger $ calcSecs clock

dateTimeToDur :: DateTime -> Maybe DateTime -> Int
dateTimeToDur start endM = case endM of
    Nothing  -> 0
    Just end -> round $
        diffUTCTime (dateTimeToUTC end) (dateTimeToUTC start)
