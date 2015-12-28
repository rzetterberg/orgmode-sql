{-|
Functionality to export different parts of the orgmode data structure tree.
All parts have functions to export them separately.
-}

module Database.OrgMode.Export.OrgParse where

import           Data.Maybe (isNothing)
import           Data.OrgMode.Parse.Types
import           Database.Persist (Entity(..))
import qualified Data.HashMap.Strict as HM

import           Database.OrgMode.Internal.Import
import qualified Database.OrgMode.Internal.Query.Clock as DbClock
import qualified Database.OrgMode.Internal.Query.Document as DbDocument
import qualified Database.OrgMode.Internal.Query.Heading as DbHeading
import qualified Database.OrgMode.Internal.Query.Planning as DbPlanning
import qualified Database.OrgMode.Internal.Query.Property as DbProperty
import qualified Database.OrgMode.Internal.Query.Tag as DbTag
import qualified Database.OrgMode.Internal.Convert.Time as TimeConvert
import qualified Database.OrgMode.Internal.Types as Db

-------------------------------------------------------------------------------

{-|
Represents the hierarchy relationship between 'Heading's for use in a flat
list. The heading, it's database ID and the optional parent database ID.
-}
type HeadingRel = (Heading, Key Db.Heading, Maybe (Key Db.Heading))

{-|
Exports a complete document along with it's headings from the database using
the given 'Db.Document' id.
-}
exportDocument :: (MonadIO m)
               => Key Db.Document                       -- ^ ID of document
               -> ReaderT SqlBackend m (Maybe Document) -- ^ Complete document
exportDocument docId = DbDocument.get docId >>= go
  where
    go Nothing    = return Nothing
    go (Just doc) = do
        dbHeadings <- DbHeading.getByDocument docId
        rels       <- mapM exportHeadingRel dbHeadings

        let res = Document (Db.documentText doc) (headingsFromRels rels)

        return (Just res)

{-|
Exports a custom document using the given list of 'Db.Heading's. Using this
function you can create custom filtering functions that retrieves headings
from the database by some constraint, then pass those headings into this
function to create a complete 'Document' and 'Heading' hierarchy.
-}
exportFilterDocument :: (MonadIO m)
                     => [Entity Db.Heading]
                     -> ReaderT SqlBackend m Document
exportFilterDocument dbHeadings = do
    rels <- mapM exportHeadingRel dbHeadings
    return $ Document "" (headingsFromRels rels)

{-|
Exports a complete heading from the database in a tuple with the database ID
and the ID of it's parent.

NB: Does not retrieve the subheadings of the given heading. All headings should
be retrieved form the database and then the hierarchy should be built using
'headingsFromRels'.
-}
exportHeadingRel :: (MonadIO m)
                 => Entity Db.Heading
                 -> ReaderT SqlBackend m HeadingRel
exportHeadingRel (Entity hedId heading) = do
    plannings  <- exportPlannings hedId
    clocks     <- exportClocks hedId
    properties <- exportProperties hedId
    tags       <- exportTags hedId

    let sec = Section { sectionPlannings  = plannings
                      , sectionClocks     = clocks
                      , sectionProperties = properties
                      , sectionParagraph  = Db.headingParagraph heading
                      }
        hed = Heading { level       = Level (Db.headingLevel heading)
                      , keyword     = StateKeyword <$> Db.headingKeyword heading
                      , priority    = Db.headingPriority heading
                      , title       = Db.headingTitle heading
                      , stats       = Nothing
                      , tags        = tags
                      , section     = sec
                      , subHeadings = []
                      }

    return (hed, hedId, (Db.headingParent heading))

{-|
Exports plannings from database for the given heading.
-}
exportPlannings :: (MonadIO m)
                => Key Db.Heading                 -- ^ ID of heading owner
                -> ReaderT SqlBackend m Plannings -- ^ Complete plannings
exportPlannings hedId = do
    dbPlannings <- (map fromDb) `liftM` DbPlanning.getByHeading hedId

    return $ Plns (HM.fromList dbPlannings)
  where
    fromDb (Entity _ p)
        = let tstamp = Timestamp start{ hourMinute = Nothing } True Nothing
              start  = TimeConvert.utcToDateTime (Db.planningTime p)
          in (Db.planningKeyword p, tstamp)

{-|
Exports clocks from the database for the given heading.
-}
exportClocks :: (MonadIO m)
             => Key Db.Heading
             -> ReaderT SqlBackend m [(Maybe Timestamp, Maybe Duration)]
exportClocks hedId = (map fromDb) `liftM` DbClock.getByHeading hedId
  where
    fromDb (Entity _ clock)
        = let tstamp  = Timestamp start (Db.clockActive clock) endM
              start   = TimeConvert.utcToDateTime (Db.clockStart clock)
              endM    = TimeConvert.utcToDateTime <$> Db.clockEnd clock
              hourMin = TimeConvert.secsToClock (Db.clockDuration clock)
          in (Just tstamp, Just hourMin)

{-|
Exports properties from the database for the given heading.
-}
exportProperties :: (MonadIO m)
                 => Key Db.Heading
                 -> ReaderT SqlBackend m Properties
exportProperties hedId
    =   DbProperty.getByHeading hedId
    >>= return . HM.fromList . map fromDb
  where
    fromDb (Entity _ p) = (Db.propertyKey p, Db.propertyValue p)

{-|
Exports tags from the database for the given heading.
-}
exportTags :: (MonadIO m)
           => Key Db.Heading
           -> ReaderT SqlBackend m [Tag]
exportTags hedId = (map fromDb) `liftM` DbTag.getByHeading hedId
  where
    fromDb (Entity _ t) = Db.tagName t

-------------------------------------------------------------------------------
-- ** Conversion

{-|
Takes a flat list of 'HeadingRel's and creates a 'Heading' hierarchy.

NB: This function is really naive and inefficient. It probably has time
complexity of O(n^∞) and you need a quantum computer to run it.
-}
headingsFromRels :: [HeadingRel] -> [Heading]
headingsFromRels rels = map findChildren $
    filter (\(_, _, parentM) -> isNothing parentM) rels
  where
    isChild currId (_, _, (Just parId)) = currId == parId
    isChild _ (_, _, Nothing)           = False
    findChildren (hed, currId, _)
        = let subs = map findChildren $ filter (isChild currId) rels
          in  hed{ subHeadings = subs }
