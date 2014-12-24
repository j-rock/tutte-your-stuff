module Main (main) where

import Data.Graph
import Control.Tutte

main :: IO ()
main = print $ findBestCycle $ completeGraph 1000
