module Main where

import           Criterion.Main (defaultMain)
import           Prelude

import qualified Query.ExportBench as Export
import qualified Query.ClockTableBench as ClockTable

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain [ Export.benches
                   , ClockTable.benches
                   ]
