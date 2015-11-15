{-|
Tests that use complete org-mode documents to verify that the whole process of
parsing and importing the data to the database works as expected.
-}

module Database.Query.HeadingSpec (spec) where

import           TestImport

import qualified Database.OrgMode.Query.Heading as Heading

-------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "getByTag" $ do
      it "example 4_sections_3_tags_2_clocks" $ do
          headings <- runDb $ do
              importExample "4_sections_3_tags_2_clocks.org"

              Heading.getByTag "support"

          (length headings) `shouldBe` 2
