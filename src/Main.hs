module Main (main) where

import Data.Graph

main :: IO ()
main = print $ findBestCycle $ hubGraph 10000
