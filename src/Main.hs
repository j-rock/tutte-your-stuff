module Main (main) where

import Graph
import Tutte

main :: IO ()
main = print $ findBestCycle $ completeGraph 1000
