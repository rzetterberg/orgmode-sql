module Main where

import           Criterion.Main (defaultMain)
import           Prelude

import qualified Model.ClockBench as ClockBench

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain [ ClockBench.benches
                   ]
