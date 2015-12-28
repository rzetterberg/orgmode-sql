{-|
Utility functions for different types of conversions between libraries and
time systems.
-}

module Database.OrgMode.Util.Time where

import           Data.OrgMode.Parse.Types
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime(..))
import qualified Data.Time.Calendar as Time
import qualified Data.Time.Clock as Time
import qualified Data.Time.Calendar.WeekDate as Time
import           Prelude

-------------------------------------------------------------------------------
-- * Calculations

{-|
Helper for calculating the duration between two 'UTCTime'. Since 'Timestamp's can
can have the end time missing, this function accepts 'Maybe UTCTime'. When the
end time is missing a duration of 0 is returned.

Duration is returned as seconds in form of an 'Int'.

20 seconds between start and end:

>>> utcToDur start (Just end)
20

No end time given:

>>> utcToDur start Nothing
0
-}
utcToDuration :: UTCTime -> Maybe UTCTime -> Int
utcToDuration start endM = case endM of
    Nothing  -> 0
    Just end -> round $ Time.diffUTCTime end start

{-|
Helper for converting a orgmode-parse 'DateTime' into a 'UTCTime'. 'UTCTime' is
used for convenience when calculating the duration of two 'DateTime's.
-}
dateTimeToUTC :: DateTime
              -> UTCTime
dateTimeToUTC (DateTime cal _ clock _ _)
    = UTCTime (Time.fromGregorian (toInteger year) month day)
              (Time.secondsToDiffTime midSecs)
  where
    (YMD' (YearMonthDay year month day)) = cal
    calcSecs Nothing       = 0
    calcSecs (Just (h, m)) = ((h * 60) + m) * 60
    midSecs                = toInteger $ calcSecs clock

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
    (year, month, day) = Time.toGregorian utctDay
    (_, _, weekDay)    = Time.toWeekDate utctDay

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
