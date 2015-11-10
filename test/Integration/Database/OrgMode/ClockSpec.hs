{-|
Tests that use complete org-mode documents to verify that the whole process of
parsing and importing the data to the database works as expected.
-}

module Integration.Database.OrgMode.ClockSpec (spec) where

import           Data.Attoparsec.Text (parseOnly)
import           TestImport
import qualified Data.OrgMode.Parse.Attoparsec.Document as OrgDoc
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Database.OrgMode
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
  describe "getTagTotal" $ do
      it "example 4_sections_3_tags_2_clocks" $ do
          clocks <- runDb $ do
              importExample "4_sections_3_tags_2_clocks.org"

              Clock.getTagTotal

          (length clocks) `shouldBe` 3
      it "example 2_clocks_45_minutes" $ do
          totals <- runDb $ do
              importExample "2_clocks_45_minutes.org"

              Clock.getTagTotal

          (length totals) `shouldBe` 1
  describe "getHeadingTotal" $ do
      it "example 4_sections_3_tags_2_clocks" $ do
          totals <- runDb $ do
              importExample "4_sections_3_tags_2_clocks.org"

              Clock.getHeadingTotal

          (length totals) `shouldBe` 4
      it "example 2_clocks_45_minutes" $ do
          totals <- runDb $ do
              importExample "2_clocks_45_minutes.org"

              Clock.getHeadingTotal

          (length totals) `shouldBe` 1
      it "example 2_sections_2_clocks_each" $ do
          totals <- runDb $ do
              importExample "2_sections_2_clocks_each.org"

              Clock.getHeadingTotal

          (length totals) `shouldBe` 2
  describe "getDocumentTotal" $ do
      it "example 2 docs, 2 clocks" $ do
          totals <- runDb $ do
              importExample "4_sections_3_tags_2_clocks.org"
              importExample "1_clock_2_hours.org"

              Clock.getDocumentTotal

          (length totals) `shouldBe` 2
      it "example 2 docs, no clocks" $ do
          totals <- runDb $ do
              importExample "1_section.org"
              importExample "2_sections.org"

              Clock.getDocumentTotal

          (length totals) `shouldBe` 2
      it "example 2 docs, 1 clock" $ do
          totals <- runDb $ do
              importExample "1_section.org"
              importExample "1_clock_2_hours.org"

              Clock.getDocumentTotal

          (length totals) `shouldBe` 2

{-|
Imports the given example file to the database
-}
importExample :: (MonadIO m) => FilePath -> ReaderT SqlBackend m ()
importExample fname = do
    contents <- liftIO (getExample fname)
    parseImport (T.pack fname) allowedTags contents
  where
    allowedTags = ["TODO", "DONE"]

{-|
Helper for reading a org-mode example file from the `examples` directory in
test.
-}
getExample :: FilePath -> IO Text
getExample fname = T.readFile $ "test/examples/" ++ fname

{-|
Helper for parsing org-mode document contents and then importing the result
into the database. Does nothing on parsing failure, instead lets the database
test constraints handle the failure (since nothing will be inserted into the
database on parse failure).
-}
parseImport :: (MonadIO m)
            => Text                    -- ^ Name of the document
            -> [Text]                  -- ^ Keywords to allow
            -> Text                    -- ^ org-mode document contents
            -> ReaderT SqlBackend m ()
parseImport docName keywords orgContent =
    case result of
        Left _    -> return ()
        Right doc -> void $ importDocument docName doc
  where
    result = parseOnly (OrgDoc.parseDocument keywords) orgContent
