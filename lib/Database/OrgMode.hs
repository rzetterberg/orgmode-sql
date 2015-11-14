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

import           Database.OrgMode.Import
import qualified Database.OrgMode.Marshall as Db
import qualified Database.OrgMode.Model as Db
import qualified Database.OrgMode.Model.Clock as Clock
import qualified Database.OrgMode.Model.Document as Document
import qualified Database.OrgMode.Model.Heading as Heading
import qualified Database.OrgMode.Model.Section as Section
import qualified Database.OrgMode.Model.Tag as Tag

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
        Left err  -> return (Left err)
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
importDocument docName doc = do
    docId <- Document.add dbDoc

    mapM_ (importHeading Nothing docId) (documentHeadings doc)

    return docId
  where
    dbDoc = Db.documentToDb docName doc

{-|
Takes a parsed heading and inserts it into the database. Since headings in
org-mode have a tree structure each 'Heading' can contain an ID to it's parent.

Headings belong to a specific document that's why the ID is needed when
inserting a 'Heading' into the database.

Note: this function recurses on each 'subHeadings' found.
-}
importHeading :: (MonadIO m)
              => Maybe (Key Db.Heading)                -- ^ ID of parent heading
              -> Key Db.Document                       -- ^ ID of document owner
              -> Heading                               -- ^ Parsed heading
              -> ReaderT SqlBackend m (Key Db.Heading) -- ^ ID of the heading
importHeading parentIdM docId heading@Heading{..} = do
    secId <- Section.add dbSection

    let dbHeading = Db.headingToDb secId parentIdM docId heading

    headingId <- Heading.add dbHeading

    let nextParentIdM = Just headingId

    mapM_ (Tag.add headingId) tags
    mapM_ (importClock secId) (sectionClocks section)
    mapM_ (importHeading nextParentIdM docId) subHeadings

    return headingId
  where
    dbSection = Db.sectionToDb section

{-|
Takes an ID to a section and a orgmode-parse Clock and converts the time data
into timestamps before inserting it into the database. Duration is calculated
before inserting the clock to ensure that all durations are up-to-date.

If you edit a timestamp manually in org-mode you need to update the the duration
with `org-evaluate-time-range` manually also. Sometimes you forget to do this
update, that's why we make sure the duration is updated by calculating it before
inserting the clock.

NB: orgmode-parse does not export the Clock-type, hence the funky tuple.
-}
importClock :: (MonadIO m)
            => Key Db.Section                    -- ^ ID of the owner
            -> (Maybe Timestamp, Maybe Duration) -- ^ orgmode-parse Clock
            -> ReaderT SqlBackend m ()
importClock secId clock
    = case Db.clockToDb secId clock of
        Nothing      -> return ()
        Just dbClock -> void $ Clock.add dbClock
