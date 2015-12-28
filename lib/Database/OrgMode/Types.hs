{-|
-}

module Database.OrgMode.Types where

import           Data.Aeson
import           Data.Default
import           Data.Int (Int64)
import           GHC.Generics

import           Database.OrgMode.Internal.Types
import           Database.OrgMode.Internal.Import

--------------------------------------------------------------------------------

{-|
Represents a summary of a 'Heading' that only contains the title and the total
duration of all clocks for the 'Heading'.

Is made to be used when displaying clock tables and other types of overviews.
-}
data HeadingShort = HeadingShort
  { headingShortDocId    :: Int64
  , headingShortId       :: Int64
  , headingShortParId    :: Maybe Int64
  , headingShortTitle    :: Text
  , headingShortDuration :: Int
  , headingShortSubs     :: [HeadingShort]
  } deriving (Show, Generic)

instance ToJSON HeadingShort

{-|
Represents a row in a 'ClockTable'. Each row consists of the name of the
document, how long the total duration is (all 'Heading's clocks combined) and
a list of 'HeadingShort's.
-}
data ClockRow = ClockRow
  { clockRowDocumentId   :: Int64          -- ^ Document all shorts belong to
  , clockRowDuration     :: Int            -- ^ Total duration of all shorts
  , clockRowShorts       :: [HeadingShort] -- ^ Matching shorts of this row
  } deriving (Show, Generic)

instance ToJSON ClockRow

{-|
Represents a clock table which contains 'ClockRow's and a date range that
'Heading's are shown based on their clockings.
-}
data ClockTable = ClockTable
  { clockTableRows   :: [ClockRow]    -- ^ Matching rows
  , clockTableFilter :: HeadingFilter -- ^ Filter used to build the table
  } deriving (Show)

{-|
Represents a filter that is used when retrieving 'Heading's, is used when
building 'ClockTable's for example.

'HeadingFilter' implements the 'Default' type class so you can use the
'def' function from "Data.Default" to supply a filter with no constraints.

Start of date range matches clocks that start on or after given date.

For example say we have this 'Heading' parsed and saved in the database:

>* This is a root section
>  CLOCK: [2015-10-12 Sun 00:00]--[2015-10-15 Sun 00:00] =>  72:00

This filter will produce a match:

> import Data.Time.Calendar (fromGregorian)
> import Data.Time.Clock (secondsToDiffTime, UTCTime(..))
>
> let startT = UTCTime (fromGregorian 2015 10 11) (secondsToDiffTime 0)
> HeadingFilter (Just startT) Nothing []

But using this filter will not:

> let startT = UTCTime (fromGregorian 2015 10 15) (secondsToDiffTime 0)
> HeadingFilter (Just startT) Nothing []

End of date range matches clocks that end on or before given date.
This filter will produce a match:

> let endT = UTCTime (fromGregorian 2015 10 14) (secondsToDiffTime 0)
> HeadingFilter Nothing (Just endT) []

But using this filter will not:

> let endT = UTCTime (fromGregorian 2015 10 16) (secondsToDiffTime 0)
> HeadingFilter Nothing (Just endT) []

This date range filter will produce a match:

> let startT = UTCTime (fromGregorian 2015 10 11) (secondsToDiffTime 0)
>     endT   = UTCTime (fromGregorian 2015 10 16) (secondsToDiffTime 0)
> HeadingFilter (Just startT) (Just endT) []

But using this filter will not:

> let startT = UTCTime (fromGregorian 2015 10 13) (secondsToDiffTime 0)
>     endT   = UTCTime (fromGregorian 2015 10 16) (secondsToDiffTime 0)
> HeadingFilter (Just startT) (Just endT) []

This will not either:

> let startT = UTCTime (fromGregorian 2015 10 11) (secondsToDiffTime 0)
>     endT   = UTCTime (fromGregorian 2015 10 14) (secondsToDiffTime 0)
> HeadingFilter (Just startT) (Just endT) []

This means when you supply a date range it will only match clocks that start
and end within the range.

Using an empty list of 'Document' IDs means matching all 'Document's.
-}
data HeadingFilter = HeadingFilter
  { headingFilterClockStart  :: Maybe UTCTime    -- ^ Start of date range
  , headingFilterClockEnd    :: Maybe UTCTime    -- ^ End of date range
  , headingFilterDocumentIds :: [(Key Document)] -- ^ Documents to get items from
  } deriving (Show)

instance Default HeadingFilter where
    def = HeadingFilter Nothing Nothing []
