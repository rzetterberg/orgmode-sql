{-|
Tests that use complete org-mode documents to verify that the whole process of
parsing and importing the data to the database works as expected.
-}

module Database.Model.DocumentSpec (spec) where

import           TestImport

import qualified Database.OrgMode.Model.Document as Document

-------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "getTotals" $ do
      it "example 2 docs, 2 clocks" $ do
          totals <- runDb $ do
              importExample "4_sections_3_tags_2_clocks.org"
              importExample "1_clock_2_hours.org"

              Document.getTotals

          (length totals) `shouldBe` 2
      it "example 2 docs, no clocks" $ do
          totals <- runDb $ do
              importExample "1_section.org"
              importExample "2_sections.org"

              Document.getTotals

          (length totals) `shouldBe` 2
      it "example 2 docs, 1 clock" $ do
          totals <- runDb $ do
              importExample "1_section.org"
              importExample "1_clock_2_hours.org"

              Document.getTotals

          (length totals) `shouldBe` 2
