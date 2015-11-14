module Main where

import           Criterion.Main (defaultMain)
import           Prelude

import qualified Query.ClockBench as ClockBench

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain [ ClockBench.benches
                   ]
