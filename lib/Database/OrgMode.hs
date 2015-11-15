{-|
= Introduction

<<https://api.travis-ci.org/rzetterberg/orgmode-sql.svg Travis CI status>>

A library that parses org-mode documents, imports the data into an SQL database,
provides the user with common queries on the data and functionality to export
the data to different data formats.

The current feature status of this library is:

- Parsing: 100%
- Database creation: 100%
- Common queries: 100%
- Complex queries: 0%
- Exporting: 0%

When the feature status is 100% a 1.0 release will be created with a stable API.
After that semantic versioning will be used.

= Goals

Provide a solid foundation to build interesting applications/services that
revolves around using org-mode data.

This library should to be usable with MySQL, PostgreSQL and SQLite.
However, currently testing is only performed on SQLite.

= Under the hood

The foundation of this library revolves around using
<https://hackage.haskell.org/package/orgmode-parse orgmode-parse> and
<https://hackage.haskell.org/package/persistent persistent>. Those two libraries
takes care of the heavy lifting and solves the real problems. This library is
just the glue between them!

If you are familiar with orgmode-parse you know that it has data types for all
the different data in an org-mode document. This library has it's own data types
that is structured similarly but adapted for storage in SQL-databases. This
library aims to not expose the user to the internal data types and instead the
user should use the data types in orgmode-parse.

In other words, you put in orgmode-parse data types and get out orgmode-parse
data types.

= How to use the library

This section is going to be created when 1.0 is released. Right now this library
should not be used!
-}

module Database.OrgMode where

import           Data.Attoparsec.Text (parseOnly)
import           Data.OrgMode.Parse.Types
import           Data.Time.Clock (UTCTime(..))
import           Database.Persist (Entity(..))
import qualified Data.HashMap.Strict as HM
import qualified Data.OrgMode.Parse.Attoparsec.Document as OrgParse
import qualified Data.Time.Calendar as T
import qualified Data.Time.Calendar.WeekDate as T
import qualified Data.Time.Clock as T

import           Database.OrgMode.Import
import qualified Database.OrgMode.Model as Db
import qualified Database.OrgMode.Query.Document as DbDocument
import qualified Database.OrgMode.Query.Heading as DbHeading
import qualified Database.OrgMode.Query.Tag as DbTag
import qualified Database.OrgMode.Query.TagRel as DbTagRel
import qualified Database.OrgMode.Query.Clock as DbClock
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
                             , headingParagraph = sectionParagraph
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
-- * Data export

{-|
Exports a complete document along with it's headings from the database.
-}
exportDocument :: (MonadIO m)
               => Key Db.Document                       -- ^ ID of document
               -> ReaderT SqlBackend m (Maybe Document) -- ^ Complete document
exportDocument docId = DbDocument.get docId >>= go
  where
    go Nothing    = return Nothing
    go (Just doc) = do
        dbHeadings <- DbHeading.getByDocument docId
        headings   <- mapM exportHeading dbHeadings

        let res = Document (Db.documentText doc) headings

        return (Just res)

{-|
-}
exportHeading :: (MonadIO m)
              => Entity Db.Heading            -- ^ ID of document owner
              -> ReaderT SqlBackend m Heading -- ^ Complete heading
exportHeading (Entity hedId heading) = do
    plannings  <- exportPlannings hedId
    clocks     <- exportClocks hedId
    properties <- exportProperties hedId
    tags       <- exportTags hedId

    let sec = Section { sectionPlannings  = plannings
                      , sectionClocks     = clocks
                      , sectionProperties = properties
                      , sectionParagraph  = Db.headingParagraph heading
                      }

    return $ Heading { level       = Level (Db.headingLevel heading)
                     , keyword     = StateKeyword <$> Db.headingKeyword heading
                     , priority    = Db.headingPriority heading
                     , title       = Db.headingTitle heading
                     , stats       = Nothing
                     , tags        = tags
                     , section     = sec
                     , subHeadings = []
                     }

exportPlannings :: (MonadIO m)
                => Key Db.Heading                 -- ^ ID of heading owner
                -> ReaderT SqlBackend m Plannings -- ^ Complete plannings
exportPlannings hedId = do
    dbPlannings <- (map fromDb) `liftM` DbPlanning.getByHeading hedId

    return $ Plns (HM.fromList dbPlannings)
  where
    fromDb (Entity _ p)
        = let tstamp = Timestamp start{ hourMinute = Nothing } True Nothing
              start  = utcToDateTime (Db.planningTime p)
          in (Db.planningKeyword p, tstamp)

exportClocks :: (MonadIO m)
             => Key Db.Heading
             -> ReaderT SqlBackend m [(Maybe Timestamp, Maybe Duration)]
exportClocks hedId = (map fromDb) `liftM` DbClock.getByHeading hedId
  where
    fromDb (Entity _ clock)
        = let tstamp  = Timestamp start (Db.clockActive clock) endM
              start   = utcToDateTime (Db.clockStart clock)
              endM    = utcToDateTime <$> Db.clockEnd clock
              (m, _)  = (Db.clockDuration clock) `divMod` 60
              (h, m') = m `divMod` 60
          in (Just tstamp, Just (h, m'))

exportProperties :: (MonadIO m)
                 => Key Db.Heading
                 -> ReaderT SqlBackend m Properties
exportProperties hedId =   DbProperty.getByHeading hedId
                       >>= return . HM.fromList . map fromDb
  where
    fromDb (Entity _ p) = (Db.propertyKey p, Db.propertyValue p)

exportTags :: (MonadIO m)
           => Key Db.Heading
           -> ReaderT SqlBackend m [Tag]
exportTags hedId = (map fromDb) `liftM` DbTag.getByHeading hedId
  where
    fromDb (Entity _ t) = Db.tagName t

-------------------------------------------------------------------------------
-- * Helpers

{-|
Helper for converting a orgmode-parse 'DateTime' into a 'UTCTime'. 'UTCTime' is
used for convenience when calculating the duration of two 'DateTime's.
-}
utcToDateTime :: UTCTime
              -> DateTime
utcToDateTime UTCTime{..}
    = DateTime { yearMonthDay = ymd
               , dayName      = Just weekDayLit
               , hourMinute   = Just (hours, minutes2)
               , repeater     = Nothing
               , delay        = Nothing
               }
  where
    (seconds, _)      = properFraction utctDayTime
    (minutes, _)      = seconds `divMod` 60
    (hours, minutes2) = minutes `divMod` 60
    ymd               = YMD' $ YearMonthDay (fromInteger y) m d
    (y, m, d)         = T.toGregorian utctDay
    (_, _, weekDay)   = T.toWeekDate utctDay
    weekDayLit
        = case weekDay of
            1 -> "Mon"
            2 -> "Tue"
            3 -> "Wed"
            4 -> "Thu"
            5 -> "Fri"
            6 -> "Sat"
            _ -> "Sun"

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
can have the end time missing, this function accepts 'Maybe' 'DateTime'. When the
end time is missing a duration of 0 is returned.
-}
utcToDur :: UTCTime -> Maybe UTCTime -> Int
utcToDur start endM = case endM of
    Nothing  -> 0
    Just end -> round $ T.diffUTCTime end start
