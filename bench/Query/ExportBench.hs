module Query.ExportBench (benches) where

import           BenchImport
import           Criterion.Types
import           Data.Attoparsec.Text (parseOnly)
import qualified Data.OrgMode.Parse.Attoparsec.Document as OrgParse
import qualified Data.Text as T

import           Database.OrgMode.Import.OrgParse (importDocument)
import           Database.OrgMode.Export.OrgParse (exportDocument)

--------------------------------------------------------------------------------

benches :: Benchmark
benches = bgroup "Export" [ benchSmallDoc
                          , benchMediumDoc
                          , benchLargeDoc
                          , benchVeryLargeDoc
                          ]

benchSmallDoc :: Benchmark
benchSmallDoc = bench "small doc" $ whnfIO $ do
    mkExportBench "1_section_2_subs.org"

benchMediumDoc :: Benchmark
benchMediumDoc = bench "medium doc" $ whnfIO $ do
    mkExportBench "4_sections_3_tags_2_clocks.org"

benchLargeDoc :: Benchmark
benchLargeDoc = bench "large doc" $ whnfIO $ do
    mkExportBench "45_sections_multi_clocks_tree.org"

benchVeryLargeDoc :: Benchmark
benchVeryLargeDoc = bench "very large doc" $ whnfIO $ do
    mkExportBench "90_sections_multi_clocks_tree.org"

--------------------------------------------------------------------------------
-- * Helpers

allowedTags :: [Text]
allowedTags = ["DONE", "TODO"]

mkExportBench :: FilePath -> IO ()
mkExportBench fname = getExample fname >>=
    (go . parseOnly (OrgParse.parseDocument allowedTags))
  where
    go (Left err)  = error $ "Example could not be parsed: " ++ err
    go (Right doc) = void $ runDb $
        importDocument (T.pack fname) doc >>= exportDocument
