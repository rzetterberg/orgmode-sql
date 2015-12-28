{-|
Specific unit tests for 'ClockTable' queries.
-}

module Database.Export.ClockTableSpec (spec) where

import           Data.Default
import           TestImport
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.Clock (secondsToDiffTime, UTCTime(..))

import           Database.OrgMode.Types
import qualified Database.OrgMode.Export.ClockTable as ClockTable

-------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "getTable" $ do
      it "1 heading, 1 doc, no filter" $ do
          table <- runDb $ do
              void $ importExample "1_clock_2_hours.org"

              ClockTable.getTable def

          (length (clockTableRows table)) `shouldBe` 1
      it "2 docs, no filter" $ do
          table <- runDb $ do
              void $ importExample "1_clock_2_hours.org"
              void $ importExample "2_sections_2_clocks_each.org"

              ClockTable.getTable def

          (length (clockTableRows table)) `shouldBe` 2
      it "all data, no filter" $ do
          table <- runDb $ do
              void $ importExample "all_data.org"

              ClockTable.getTable def

          let rows = clockTableRows table

          (length rows) `shouldBe` 1

          let (row1:_) = rows
              shorts   = clockRowShorts row1

          (length shorts) `shouldBe` 1
  describe "getShorts" $ do
      it "1 heading, 1 doc, no filter" $ do
          parts <- runDb $ do
              void $ importExample "1_clock_2_hours.org"

              ClockTable.getShorts def

          (length parts) `shouldBe` 1
      it "2 docs, filter by 1 doc" $ do
          parts <- runDb $ do
              void $ importExample "1_clock_2_hours.org"
              docId2 <- importExample "2_sections_2_clocks_each.org"

              let f = def{ headingFilterDocumentIds = [docId2] }

              ClockTable.getShorts f

          (length parts) `shouldBe` 2
      it "3 docs, filter by 2 docs" $ do
          parts <- runDb $ do
              void $ importExample "1_clock_2_hours.org"
              docId2 <- importExample "2_sections_2_clocks_each.org"
              docId3 <- importExample "2_clocks_45_minutes.org"

              let f = def{ headingFilterDocumentIds = [docId2, docId3] }

              ClockTable.getShorts f

          (length parts) `shouldBe` 3
      it "2 docs, filter by start date after both" $ do
          let startT = UTCTime (fromGregorian 2050 1 1) (secondsToDiffTime 0)

          parts <- runDb $ do
              void $ importExample "1_clock_2_hours.org"
              void $ importExample "2_sections_2_clocks_each.org"

              let f = def{ headingFilterClockStart = Just startT }

              ClockTable.getShorts f

          (length parts) `shouldBe` 0
      it "2 docs, filter by start date before first" $ do
          let startT = UTCTime (fromGregorian 2015 10 16) (secondsToDiffTime 0)

          parts <- runDb $ do
              void $ importExample "1_clock_2_hours.org"
              void $ importExample "2_sections_2_clocks_each.org"

              let f = def{ headingFilterClockStart = Just startT }

              ClockTable.getShorts f

          (length parts) `shouldBe` 2
      it "2 docs, filter by end date before both" $ do
          let endT = UTCTime (fromGregorian 1999 1 1) (secondsToDiffTime 0)

          parts <- runDb $ do
              void $ importExample "1_clock_2_hours.org"
              void $ importExample "2_sections_2_clocks_each.org"

              let f = def{ headingFilterClockEnd = Just endT }

              ClockTable.getShorts f

          (length parts) `shouldBe` 0
      it "2 docs, filter by end date after first" $ do
          let endT = UTCTime (fromGregorian 2015 10 16) (secondsToDiffTime 0)

          parts <- runDb $ do
              void $ importExample "1_clock_2_hours.org"
              void $ importExample "2_sections_2_clocks_each.org"

              let f = def{ headingFilterClockEnd = Just endT }

              ClockTable.getShorts f

          (length parts) `shouldBe` 1
      it "2 docs, filter by start and end date for first" $ do
          let startT = UTCTime (fromGregorian 2015 10 14) (secondsToDiffTime 0)
              endT   = UTCTime (fromGregorian 2015 10 16) (secondsToDiffTime 0)

          parts <- runDb $ do
              void $ importExample "1_clock_2_hours.org"
              void $ importExample "2_sections_2_clocks_each.org"

              let f = def{ headingFilterClockStart = Just startT
                         , headingFilterClockEnd   = Just endT
                         }

              ClockTable.getShorts f

          (length parts) `shouldBe` 1
      it "2 docs, filter by start and end date for second" $ do
          let startT = UTCTime (fromGregorian 2015 10 16) (secondsToDiffTime 0)
              endT   = UTCTime (fromGregorian 2015 10 20) (secondsToDiffTime 0)

          parts <- runDb $ do
              void $ importExample "1_clock_2_hours.org"
              void $ importExample "2_sections_2_clocks_each.org"

              let f = def{ headingFilterClockStart = Just startT
                         , headingFilterClockEnd   = Just endT
                         }

              ClockTable.getShorts f

          (length parts) `shouldBe` 2
