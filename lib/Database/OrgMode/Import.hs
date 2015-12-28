{-|
Functionality to import different parts of the orgmode data structure tree.
All parts have functions to import them separately.
-}

module Database.OrgMode.Import where

import qualified Data.OrgMode.Parse.Attoparsec.Document as OrgParse
import           Data.Attoparsec.Text (parseOnly)
import           Data.OrgMode.Parse.Types
import           Data.Text (strip, append)
import qualified Data.HashMap.Strict as HM

import           Database.OrgMode.Internal.Import
import qualified Database.OrgMode.Query.Clock as DbClock
import qualified Database.OrgMode.Query.Document as DbDocument
import qualified Database.OrgMode.Query.Heading as DbHeading
import qualified Database.OrgMode.Query.Planning as DbPlanning
import qualified Database.OrgMode.Query.Property as DbProperty
import qualified Database.OrgMode.Query.Tag as DbTag
import qualified Database.OrgMode.Query.TagRel as DbTagRel
import qualified Database.OrgMode.Types as Db
import qualified Database.OrgMode.Util.Time as TimeUtil

-------------------------------------------------------------------------------
-- * Plain text

{-|
Takes a strict 'Text' and tries to parse the document and import it to the
database.

Returns the document ID of the created document or the error message from
the parser.
-}
textImportDocument :: (MonadIO m)
                   => Text                    -- ^ Name of the document
                   -> [Text]                  -- ^ Keywords to allow
                   -> Text                    -- ^ org-mode document contents
                   -> ReaderT SqlBackend m (Either String (Key Db.Document))
textImportDocument docName keywords orgContent =
    case result of
        Left  err -> return (Left err)
        Right doc -> Right `liftM` importDocument docName doc
  where
    result = parseOnly (OrgParse.parseDocument keywords)
                       (append orgContent "\n")

-------------------------------------------------------------------------------
-- * Orgmode-parse types

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
    = DbClock.add headingId tsActive start endM >>= return . Just
  where
    Timestamp{..} = tstamp
    start = TimeUtil.dateTimeToUTC tsTime
    endM  = TimeUtil.dateTimeToUTC <$> tsEndTime

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
    start = TimeUtil.dateTimeToUTC tsTime

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
