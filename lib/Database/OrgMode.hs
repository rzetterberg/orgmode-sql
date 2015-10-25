{-|
Functionality to import a parsed org-mode document into a SQL database. Does not
contain any functionality to perform the parsing, only to convert the types from
the orgmode-parse library into database friendly types. Oh, it also does the
actual insertion of the data into the database.

The module is divided into two parts:

- Public API
- Private API

Public API contains all the functions that is needed to use this library as an
end-user. But, sometimes you need to access to underlying functions too. That's
why all functions are exported from the module, but divided into two parts to
try and keep it simple.

<-- Here the usage examples will be shown -->
-}

module Database.OrgMode where

import           Data.OrgMode.Parse.Types

import           Database.OrgMode.Import
import qualified Database.OrgMode.Marshall as Db
import qualified Database.OrgMode.Model as Db
import qualified Database.OrgMode.Model.Clock as Clock
import qualified Database.OrgMode.Model.Document as Document
import qualified Database.OrgMode.Model.Heading as Heading
import qualified Database.OrgMode.Model.Section as Section
import qualified Database.OrgMode.Model.Tag as Tag

-------------------------------------------------------------------------------
-- * Public API

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
               -> ReaderT SqlBackend m (Key Db.Document) -- ^ ID of the inserted
importDocument docName doc = do
    docId <- Document.add dbDoc

    mapM_ (importHeading Nothing docId) (documentHeadings doc)

    return docId
  where
    dbDoc = Db.documentToDb docName doc

-------------------------------------------------------------------------------
-- * Private API

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

NB: orgmode-parse does not expose the Clock-type, hence the funky tuple.
-}
importClock :: (MonadIO m)
            => Key Db.Section                    -- ^ ID of the owner
            -> (Maybe Timestamp, Maybe Duration) -- ^ orgmode-parse Clock
            -> ReaderT SqlBackend m ()
importClock secId clock
    = case Db.clockToDb secId clock of
        Nothing      -> return ()
        Just dbClock -> void $ Clock.add dbClock
