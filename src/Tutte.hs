{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Tutte
  (
    startTutte
  , TutteState(..)
  , advanceTutte
  ) where

import Control.Arrow ((***))
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Graph (vertices, Graph, Vertex, findBestCycle, neighbors)

type Set = S.Set
type Map = M.Map

type Vector2 = (Float, Float)

data TutteState =
    Tutte
    { graph :: Graph
    , fixedVerts :: Set Vertex
    , vertPositions :: Map Vertex Vector2
    } deriving (Show)

startTutte :: Graph -> TutteState
startTutte graph =
    let vertPositions = makeRing cycleVerts
                          `M.union`
                        (M.fromList $ zip nonCycleVerts (repeat (0.0, 0.0)))
    in Tutte{graph, fixedVerts, vertPositions}
  where cycleVerts = findBestCycle graph
        fixedVerts = S.fromList cycleVerts
        nonCycleVerts = filter (\v -> not $ S.member v fixedVerts) $ vertices graph


makeRing :: [Vertex] -> Map Vertex Vector2
makeRing vs = let numVs = fromIntegral $ length vs
                  dtheta = (2 * 3.14159265358) / numVs
              in M.fromList $ map (id *** circle dtheta numVs) $ zip vs [0.0..]
  where circle :: Float -> Float -> Float -> Vector2
        circle dtheta radius i = let i' = i * dtheta
                                 in (radius * cos i', radius * sin i')

-- This is inherently parallel...
-- advanceTutte is O(EVlogV)
-- could be O(EVlogV/p) with p cores
advanceTutte :: TutteState -> TutteState
advanceTutte t@Tutte{..} =
    let newPos = M.mapWithKey findNewPosition vertPositions
    in t{vertPositions = newPos}
  where findNewPosition v oldPos|S.member v fixedVerts = oldPos
                                |otherwise = baryCenter $ neighborPos v
        neighborPos v = map (vertPositions M.!) $ neighbors graph v

baryCenter :: [Vector2] -> Vector2
baryCenter vecs = go vecs (0.0,0.0) 0.0
  where go []            (x, y) l = let minL = max 1.0 l in (x / minL, y / minL)
        go ((vx, vy):vs) (x, y) l = go vs (x+vx,y+vy) (l+1.0)
