{-|
-}

module Database.OrgMode.Export where

import           Data.OrgMode.Parse.Types
import           Data.Time.Clock (UTCTime(..))
import           Database.Persist (Entity(..))
import qualified Data.HashMap.Strict as HM
import qualified Data.Time.Calendar as T
import qualified Data.Time.Calendar.WeekDate as T

import           Database.OrgMode.Internal.Import
import qualified Database.OrgMode.Query.Clock as DbClock
import qualified Database.OrgMode.Query.Document as DbDocument
import qualified Database.OrgMode.Query.Heading as DbHeading
import qualified Database.OrgMode.Query.Planning as DbPlanning
import qualified Database.OrgMode.Query.Property as DbProperty
import qualified Database.OrgMode.Query.Tag as DbTag
import qualified Database.OrgMode.Types as Db

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
        dbHeadings <- DbHeading.getRootsByDocument docId
        headings   <- mapM exportHeading dbHeadings

        let res = Document (Db.documentText doc) headings

        return (Just res)

{-|
Exports a complete heading from the database in a tuple with the database ID
and the ID of it's parent.
-}
exportHeading :: (MonadIO m)
              => Entity Db.Heading
              -> ReaderT SqlBackend m Heading
exportHeading (Entity hedId heading) = do
    plannings  <- exportPlannings hedId
    clocks     <- exportClocks hedId
    properties <- exportProperties hedId
    tags       <- exportTags hedId
    subs       <- DbHeading.getChildren hedId >>= mapM exportHeading

    let sec = Section { sectionPlannings  = plannings
                      , sectionClocks     = clocks
                      , sectionProperties = properties
                      , sectionParagraph  = Db.headingParagraph heading
                      }

    return Heading { level       = Level (Db.headingLevel heading)
                   , keyword     = StateKeyword <$> Db.headingKeyword heading
                   , priority    = Db.headingPriority heading
                   , title       = Db.headingTitle heading
                   , stats       = Nothing
                   , tags        = tags
                   , section     = sec
                   , subHeadings = subs
                   }

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
              start  = utcToDateTime (Db.planningTime p)
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
              start   = utcToDateTime (Db.clockStart clock)
              endM    = utcToDateTime <$> Db.clockEnd clock
              hourMin = secsToClock (Db.clockDuration clock)
          in (Just tstamp, Just hourMin)

{-|
Exports properties from the database for the given heading.
-}
exportProperties :: (MonadIO m)
                 => Key Db.Heading
                 -> ReaderT SqlBackend m Properties
exportProperties hedId =   DbProperty.getByHeading hedId
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
Helper for converting a orgmode-parse 'DateTime' into a 'UTCTime'. 'UTCTime' is
used for convenience when calculating the duration of two 'DateTime's.
-}
utcToDateTime :: UTCTime
              -> DateTime
utcToDateTime UTCTime{..}
    = DateTime { yearMonthDay = ymd
               , dayName      = Just (weekDayToLit weekDay)
               , hourMinute   = Just (secsToClock seconds)
               , repeater     = Nothing
               , delay        = Nothing
               }
  where
    ymd                = YMD' $ YearMonthDay (fromInteger year) month day
    (seconds, _)       = properFraction utctDayTime
    (year, month, day) = T.toGregorian utctDay
    (_, _, weekDay)    = T.toWeekDate utctDay

{-|
Helper to convert seconds into a clock (hour and minute).

>>> secsToClock (120 :: Int)
(0, 2)
-}
secsToClock :: (Integral a) => a -> (a, a)
secsToClock seconds
    = let (minutes, _)         = seconds `divMod` 60
          (hours, restMinutes) = minutes `divMod` 60
      in  (hours, restMinutes)

{-|
Helper to convert a week day number into a literal representation.

>>> weekDayToLit 1
"Mon"
-}
weekDayToLit :: Int -> Text
weekDayToLit 1 = "Mon"
weekDayToLit 2 = "Tue"
weekDayToLit 3 = "Wed"
weekDayToLit 4 = "Thu"
weekDayToLit 5 = "Fri"
weekDayToLit 6 = "Sat"
weekDayToLit _ = "Sun"
