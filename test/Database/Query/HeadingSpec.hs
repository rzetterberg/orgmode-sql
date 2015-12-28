{-|
Tests that use complete org-mode documents to verify that the whole process of
parsing and importing the data to the database works as expected.
-}

module Database.Query.HeadingSpec (spec) where

import           TestImport

import qualified Database.OrgMode.Internal.Query.Heading as Heading

-------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "getByTag" $ do
      it "example 4_sections_3_tags_2_clocks" $ do
          headings <- runDb $ do
              void $ importExample "4_sections_3_tags_2_clocks.org"

              Heading.getByTag "support"

          (length headings) `shouldBe` 2
  describe "deleteByIds" $ do
      it "4 headings, delete first one" $ do
          let extractIds = map (\(Entity hedId _) -> hedId)

          (beforeLen, afterLen) <- runDb $ do
              void $ importExample "4_sections_3_tags_2_clocks.org"

              heds1 <- Heading.getAll

              let hedIds1 = extractIds heds1

              Heading.deleteByIds [head hedIds1]

              heds2 <- Heading.getAll

              let hedIds2 = extractIds heds2

              return (length hedIds1, length hedIds2)

          beforeLen `shouldBe` 4
          afterLen `shouldBe` 3
  describe "deleteByDocuments" $ do
      it "2 documents, delete first one" $ do
          (beforeLen, afterLen) <- runDb $ do
              docId1 <- importExample "1_section.org"
              void $ importExample "4_sections_3_tags_2_clocks.org"

              heds1 <- Heading.getAll

              Heading.deleteByDocuments [docId1]

              heds2 <- Heading.getAll

              return (length heds1, length heds2)

          beforeLen `shouldBe` 5
          afterLen `shouldBe` 4
