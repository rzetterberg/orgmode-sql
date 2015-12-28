{-|
Tests that use complete org-mode documents to verify that the whole process of
parsing and importing the data to the database works as expected.
-}

module Database.Query.ClockSpec (spec) where

import           TestImport

import qualified Database.OrgMode.Internal.Query.Heading as Heading
import qualified Database.OrgMode.Internal.Query.Clock as Clock

-------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "getByHeading" $ do
      it "example 1_clock_2_hours" $ do
          clocks <- runDb $ do
              void $ importExample "1_clock_2_hours.org"

              headings <- Heading.getAll

              let (Entity hedId1 _):_ = headings

              Clock.getByHeading hedId1

          (length clocks) `shouldBe` 1
      it "example 2_clocks_45_minutes" $ do
          clocks <- runDb $ do
              void $ importExample "2_clocks_45_minutes.org"

              headings <- Heading.getAll

              let (Entity hedId1 _):_ = headings

              Clock.getByHeading hedId1

          (length clocks) `shouldBe` 2
  describe "getByTag" $ do
      it "example 4_sections_3_tags_2_clocks" $ do
          clocks <- runDb $ do
              void $ importExample "4_sections_3_tags_2_clocks.org"

              Clock.getByTag "support"

          (length clocks) `shouldBe` 1
