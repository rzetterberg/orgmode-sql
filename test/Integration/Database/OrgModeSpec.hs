{-|
Tests that use complete org-mode documents to verify that the whole process of
parsing and importing the data to the database works as expected.
-}

module Integration.Database.OrgModeSpec (spec) where

import qualified Data.OrgMode.Parse.Attoparsec.Document as OrgDoc
import           Data.Attoparsec.Text (parseOnly)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import           TestImport

import           Database.OrgMode
import qualified Database.OrgMode.Model.Clock as Clock
import qualified Database.OrgMode.Model.Section as Section
import qualified Database.OrgMode.Model.Tag as Tag

-------------------------------------------------------------------------------

spec :: Spec
spec =
  describe "importDocument" $ do
      mkLengthTest "1_section.org" 1 0 0
      mkLengthTest "2_sections.org" 2 0 0
      mkLengthTest "2_sections_1_tag.org" 2 1 0
      mkLengthTest "4_sections_3_tags.org" 4 3 0
      mkLengthTest "4_sections_3_tags_1_clock.org" 4 3 1
      mkLengthTest "4_sections_3_tags_2_clocks.org" 4 3 2
      mkLengthTest "1_section_2_subs.org" 3 0 0

      mkDurationTest "1_clock_2_hours.org" 7200
      mkDurationTest "2_clocks_45_minutes.org" 2700

allowedTags :: [Text]
allowedTags = ["DONE", "TODO"]

{-|
Creates a generic test that loads an org-mode file and checks that the total
duration of the clocks is as expected.
-}
mkDurationTest :: FilePath -> Int -> SpecWith ()
mkDurationTest fname expDur = it ("duration example, " ++ fname) $ do
    content <- getExample fname

    dur <- runDb $ do
        parseImport (T.pack fname) allowedTags content
        Clock.getTotalDuration Clock.getAll

    dur `shouldBe` expDur

{-|
Creates a generic test that loads an org-mode file and checks that the database
will be populated with the given amount of sections, tags and clocks.
-}
mkLengthTest :: FilePath -> Int -> Int -> Int -> SpecWith ()
mkLengthTest fname secLen tagLen clockLen = it ("length example, " ++ fname) $ do
    content <- getExample fname

    (secs, tags, clocks) <- runDb $ do
        parseImport (T.pack fname) allowedTags content

        s <- Section.getAll
        t <- Tag.getAll
        c <- Clock.getAll

        return (s, t, c)

    length secs `shouldBe` secLen
    length tags `shouldBe` tagLen
    length clocks `shouldBe` clockLen

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
