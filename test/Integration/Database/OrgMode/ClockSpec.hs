{-|
Tests that use complete org-mode documents to verify that the whole process of
parsing and importing the data to the database works as expected.
-}

module Integration.Database.OrgMode.ClockSpec (spec) where

import           TestImport

import           Database.OrgMode.Model
import qualified Database.OrgMode.Model.Heading as Heading
import qualified Database.OrgMode.Model.Clock as Clock

-------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "getByHeading" $ do
      it "example 1_clock_2_hours" $ do
          clocks <- runDb $ do
              importExample "1_clock_2_hours.org"

              headings <- Heading.getAll

              let (Entity hedId1 _):_ = headings

              Clock.getByHeading hedId1

          (length clocks) `shouldBe` 1

          let (Entity _ clock1):_ = clocks

          (clockDuration clock1) `shouldBe` (2 * 60 * 60)
      it "example 2_clocks_45_minutes" $ do
          clocks <- runDb $ do
              importExample "2_clocks_45_minutes.org"

              headings <- Heading.getAll

              let (Entity hedId1 _):_ = headings

              Clock.getByHeading hedId1

          (length clocks) `shouldBe` 2
  describe "getByTag" $ do
      it "example 4_sections_3_tags_2_clocks" $ do
          clocks <- runDb $ do
              importExample "4_sections_3_tags_2_clocks.org"

              Clock.getByTag "support"

          (length clocks) `shouldBe` 1
