{-|
Tests that use complete org-mode documents to verify that the whole process of
parsing and importing the data to the database works as expected.
-}

module Database.ExampleSpec (spec) where

import qualified Data.Text as T
import           TestImport

import qualified Database.OrgMode.Query.Clock as Clock
import qualified Database.OrgMode.Query.Section as Section
import qualified Database.OrgMode.Query.Tag as Tag

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
