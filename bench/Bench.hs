module Main where

import           Criterion.Main (defaultMain)
import           Prelude

import qualified Query.ExportBench as Export

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain [ Export.benches
                   ]
