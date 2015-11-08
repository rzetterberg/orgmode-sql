{-|
Tests that use complete org-mode documents to verify that the whole process of
parsing and importing the data to the database works as expected.
-}

module Integration.Database.OrgMode.ClockSpec (spec) where

import           Data.Time.Clock (getCurrentTime)
import           TestImport

import           Database.OrgMode.Model
import qualified Database.OrgMode.Model.Document as Document
import qualified Database.OrgMode.Model.Heading as Heading
import qualified Database.OrgMode.Model.Section as Section
import qualified Database.OrgMode.Model.Tag as Tag
import qualified Database.OrgMode.Model.Clock as Clock

-------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "getByHeading" $ do
      it "separate docs, 1 heading, 1 tag" $ do
          currT <- getCurrentTime

          clocks <- runDb $ do
              docId1 <- Document.add (Document "doc1" "")
              docId2 <- Document.add (Document "doc2" "")
              secId1 <- Section.add (Section "sec1")
              secId2 <- Section.add (Section "sec2")

              hedId1 <- Heading.add $
                  Heading 0 Nothing "head1" secId1 Nothing docId1

              void $ Heading.add $
                  Heading 0 Nothing "head2" secId2 Nothing docId2

              void $ Clock.add (Clock secId1 currT Nothing 0)
              void $ Clock.add (Clock secId2 currT Nothing 0)

              Clock.getByHeading hedId1

          (length clocks) `shouldBe` 1
  describe "getByTag" $ do
      it "separate docs, 1 heading, 1 tag" $ do
          currT <- getCurrentTime

          clocks <- runDb $ do
              docId1 <- Document.add (Document "doc1" "")
              docId2 <- Document.add (Document "doc2" "")
              secId1 <- Section.add (Section "sec1")
              secId2 <- Section.add (Section "sec2")

              hedId1 <- Heading.add $
                  Heading 0 Nothing "head1" secId1 Nothing docId1

              hedId2 <- Heading.add $
                  Heading 0 Nothing "head2" secId2 Nothing docId2

              void $ Clock.add (Clock secId1 currT Nothing 0)
              void $ Clock.add (Clock secId2 currT Nothing 0)

              void $ Tag.add hedId1 "tag1"
              void $ Tag.add hedId1 "tag2"
              void $ Tag.add hedId2 "tag3"

              Clock.getByTag "tag1"

          (length clocks) `shouldBe` 1
  describe "getTagTotal" $ do
      it "separate docs, 1 heading, 1 tag" $ do
          currT <- getCurrentTime

          totals <- runDb $ do
              docId1 <- Document.add (Document "doc1" "")
              docId2 <- Document.add (Document "doc2" "")
              secId1 <- Section.add (Section "sec1")
              secId2 <- Section.add (Section "sec2")

              hedId1 <- Heading.add $
                  Heading 0 Nothing "head1" secId1 Nothing docId1

              hedId2 <- Heading.add $
                  Heading 0 Nothing "head2" secId2 Nothing docId2

              void $ Clock.add (Clock secId1 currT Nothing 0)
              void $ Clock.add (Clock secId2 currT Nothing 0)

              void $ Tag.add hedId1 "tag1"
              void $ Tag.add hedId1 "tag2"
              void $ Tag.add hedId2 "tag3"

              Clock.getTagTotal

          (length totals) `shouldBe` 3
  describe "getHeadingTotal" $ do
      it "separate docs, 1 heading, 1 tag" $ do
          currT <- getCurrentTime

          totals <- runDb $ do
              docId1 <- Document.add (Document "doc1" "")
              docId2 <- Document.add (Document "doc2" "")
              secId1 <- Section.add (Section "sec1")
              secId2 <- Section.add (Section "sec2")

              hedId1 <- Heading.add $
                  Heading 0 Nothing "head1" secId1 Nothing docId1

              hedId2 <- Heading.add $
                  Heading 0 Nothing "head2" secId2 Nothing docId2

              void $ Clock.add (Clock secId1 currT Nothing 0)
              void $ Clock.add (Clock secId2 currT Nothing 0)

              void $ Tag.add hedId1 "tag1"
              void $ Tag.add hedId1 "tag2"
              void $ Tag.add hedId2 "tag3"

              Clock.getHeadingTotal

          (length totals) `shouldBe` 2
  describe "getDocumentTotal" $ do
      it "separate docs, 1 heading, 1 tag" $ do
          currT <- getCurrentTime

          totals <- runDb $ do
              docId1 <- Document.add (Document "doc1" "")
              docId2 <- Document.add (Document "doc2" "")
              secId1 <- Section.add (Section "sec1")
              secId2 <- Section.add (Section "sec2")

              hedId1 <- Heading.add $
                  Heading 0 Nothing "head1" secId1 Nothing docId1

              hedId2 <- Heading.add $
                  Heading 0 Nothing "head2" secId2 Nothing docId2

              void $ Clock.add (Clock secId1 currT Nothing 0)
              void $ Clock.add (Clock secId2 currT Nothing 0)

              void $ Tag.add hedId1 "tag1"
              void $ Tag.add hedId1 "tag2"
              void $ Tag.add hedId2 "tag3"

              Clock.getDocumentTotal

          (length totals) `shouldBe` 2
