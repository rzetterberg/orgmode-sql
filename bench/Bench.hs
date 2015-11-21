module Main where

import           Criterion.Main (defaultMain)
import           Prelude

import qualified Query.ImportExportBench as ImportExport

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain [ ImportExport.benches
                   ]
