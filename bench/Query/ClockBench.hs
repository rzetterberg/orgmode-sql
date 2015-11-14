module Query.ClockBench (benches) where

import           BenchImport
import           Criterion.Types
import           Data.Time.Clock (getCurrentTime)

import           Database.OrgMode.Model
import qualified Database.OrgMode.Query.Clock as Clock
import qualified Database.OrgMode.Query.Document as Document
import qualified Database.OrgMode.Query.Heading as Heading
import qualified Database.OrgMode.Query.Section as Section
import qualified Database.OrgMode.Query.Tag as Tag

--------------------------------------------------------------------------------

benches :: Benchmark
benches = bgroup "Clock" [ benchGetByTag
                         ]

benchGetByTag :: Benchmark
benchGetByTag = bench "getByTag" $ whnfIO $ do
    currT <- getCurrentTime

    runDb $ do
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
