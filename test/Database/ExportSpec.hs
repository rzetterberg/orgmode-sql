{-|
Tests that use complete org-mode documents to verify that the whole process of
parsing and importing the data to the database works as expected.
-}

module Database.ExportSpec (spec) where

import           TestImport
import           Data.OrgMode.Parse.Types

import qualified Database.OrgMode as OrgDb

-------------------------------------------------------------------------------

spec :: Spec
spec =
  describe "exportDocument" $ do
      it "root headings right amount" $ do
          (Just doc) <- runDb $ do
              docId <- importExample "2_sections.org"

              OrgDb.exportDocument docId

          let headings = documentHeadings doc

          (countHeadings headings) `shouldBe` 2
      it "hierarchy right amount" $ do
          (Just doc) <- runDb $ do
              docId <- importExample "all_data.org"

              OrgDb.exportDocument docId

          let headings = documentHeadings doc

          (countHeadings headings) `shouldBe` 3

countHeadings :: [Heading] -> Int
countHeadings []     = 0
countHeadings (h:hs) = 1 + (countHeadings (subHeadings h)) + (countHeadings hs)
