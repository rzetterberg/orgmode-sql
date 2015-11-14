{-|
Tests that use complete org-mode documents to verify that the whole process of
parsing and importing the data to the database works as expected.
-}

module Database.Query.HeadingSpec (spec) where

import           Data.Time.Clock (getCurrentTime)
import           TestImport

import           Database.OrgMode.Model
import qualified Database.OrgMode.Query.Document as Document
import qualified Database.OrgMode.Query.Heading as Heading
import qualified Database.OrgMode.Query.Tag as Tag
import qualified Database.OrgMode.Query.Clock as Clock

-------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "getByTag" $ do
      it "example 4_sections_3_tags_2_clocks" $ do
          headings <- runDb $ do
              importExample "4_sections_3_tags_2_clocks.org"

              Heading.getByTag "support"

          (length headings) `shouldBe` 2
