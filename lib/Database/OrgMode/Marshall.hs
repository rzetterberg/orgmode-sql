{-|
Functionality to convert orgmode-parse types between the types used in this
library to save the data in the database.

Currently only contains functionlity to convert into database types.
-}

module Database.OrgMode.Marshall where

import           Data.OrgMode.Parse.Types
import           Data.Time.Clock (UTCTime(..))
import           Data.Time.Clock (diffUTCTime)
import qualified Data.Time.Calendar as T
import qualified Data.Time.Clock as T

import           Database.OrgMode.Import
import qualified Database.OrgMode.Model as Db

--------------------------------------------------------------------------------

type Clock = (Maybe Timestamp, Maybe Duration)

--------------------------------------------------------------------------------

documentToDb :: Text -> Document -> Db.Document
documentToDb name Document{..} = Db.Document name documentText

sectionToDb :: Section -> Db.Section
sectionToDb Section{..} = Db.Section sectionParagraph

headingToDb :: Key Db.Section
            -> Maybe (Key Db.Heading)
            -> Key Db.Document
            -> Heading
            -> Db.Heading
headingToDb secId parIdM docId Heading{..}
    = Db.Heading level' keyword' title secId parIdM docId
  where
    (Level level') = level
    keyword'       = unStateKeyword <$> keyword

clockToDb :: Key Db.Section -> Clock -> Maybe Db.Clock
clockToDb _ (Nothing, _)         = Nothing
clockToDb secId (Just tstamp, _) = Just clock
  where
    clock         = Db.Clock secId start endM duration
    Timestamp{..} = tstamp
    start         = dateTimeToUTC tsTime
    endM          = dateTimeToUTC <$> tsEndTime
    duration :: Int
    duration = case endM of
        Nothing  -> 0
        Just end -> round $ diffUTCTime end start

{-|
Helper that takes date information from a orgmode-attoparsec document and
converts it to a 'UTCTime' that can be saved in the database.
-}
dateTimeToUTC :: DateTime -> UTCTime
dateTimeToUTC (DateTime cal _ clock _ _)
    = UTCTime (T.fromGregorian (toInteger year) month day)
              (T.secondsToDiffTime midSecs)
  where
    (YMD' (YearMonthDay year month day)) = cal
    calcSecs Nothing       = 0
    calcSecs (Just (h, m)) = ((h * 60) + m) * 60
    midSecs                = toInteger $ calcSecs clock
