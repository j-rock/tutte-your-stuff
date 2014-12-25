{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}


-- A lot of the code down here is copied from FruRein...
-- The algorithms are similar. As of now, there is little
-- need to abstract over the commonality.

module Walshaw
  (
    walshaw
  , Walshaw
  ) where

import Control.Monad.Random
import qualified Data.Map.Strict as M
import Graph (vertices, Graph, Vertex(..), neighbors)
import Tutte (ForceAlgo(..))


type Map = M.Map
type R = Float
type Vector2 = (R, R)

data Walshaw =
    Walshaw
    { graph :: Graph
    , k, t  :: R
    , verts :: Map Vertex Vector2
    } deriving (Show)


instance ForceAlgo Walshaw where
    positions = verts
    advance = advanceWalshaw

walshaw :: Graph -> Walshaw
walshaw graph =
    let
        randPosition :: RandomGen g => Rand g Vector2
        randPosition = do
          a <- getRandomR (0.0, 1.0)
          b <- getRandomR (0.0, 1.0)
          return (a, b)

        randPositions :: [Vector2]
        randPositions = evalRand (sequence $ repeat randPosition) (mkStdGen 6)

        verts = M.fromList $ zip vs randPositions
    in Walshaw{graph, k, t, verts}
  where vs = vertices graph
        numV = length vs
        k = sqrt $ 4.0 / (fromIntegral numV)
        t = 1.0


advanceWalshaw :: Walshaw -> Walshaw
advanceWalshaw f@Walshaw{..} =
    let
        disp   = M.fromList $ zip (vertices graph) $ repeat (0.0, 0.0)
        verts' = applyDisp f . local f . global f $ disp
        t' = t + 1
    in f{t = t', verts = verts'}


type DispMap = Map Vertex Vector2
type PosMap = Map Vertex Vector2

global :: Walshaw -> DispMap -> DispMap
global Walshaw{..} disp =
    let
        vdisp :: Vertex -> Vector2 -> Vector2
        vdisp v oldDisp = foldr (f v) oldDisp $ vertices graph

        f :: Vertex -> Vertex -> Vector2 -> Vector2
        f v u vd|v == u = vd
                |otherwise =
                   let (delta, dist) = getDelta verts u v
                       uMag          = mag $ verts M.! u
                   in vd `plus` (scale delta $ fg dist uMag)

        fg x w = - w * k * k / x

    in M.mapWithKey vdisp disp


local :: Walshaw -> DispMap -> DispMap
local Walshaw{..} disp =
    let
        vdisp :: Vertex -> Vector2 -> Vector2
        vdisp v oldDisp = foldr (f v) oldDisp $ neighbors graph v

        f :: Vertex -> Vertex -> Vector2 -> Vector2
        f v u vd = let (delta, dist) = getDelta verts u v
                       uMag          = mag $ verts M.! u
                       numE          = fromIntegral $ length $ neighbors graph v
                   in vd `plus` (scale delta $ fl dist numE uMag)

        fl x d w = (x - k) / d + w * k * k /x

    in M.mapWithKey vdisp disp

getDelta :: PosMap -> Vertex -> Vertex -> (Vector2, R)
getDelta pos u v = let delta = minus (pos M.! u) (pos M.! v)
                       dist  = mag delta
                   in (delta, dist)

applyDisp :: Walshaw -> DispMap -> PosMap
applyDisp Walshaw{..} disp =
    let
        temp = 0.4 / t

        update :: Vertex -> Vector2 -> Vector2
        update v pos = let vd   = disp M.! v
                           dist = mag vd
                           (px, py) = plus pos $ scale vd (min dist temp / dist)
                           px' = (-1) `max` (px `min` 1)
                           py' = (-1) `max` (py `min` 1)
                       in (px', py')

    in M.mapWithKey update verts



minus :: Vector2 -> Vector2 -> Vector2
minus (x,y) (x', y') = (x-x', y-y')

plus :: Vector2 -> Vector2 -> Vector2
plus (x,y) (x', y') = (x+x', y+y')

scale :: Vector2 -> R -> Vector2
scale (x,y) k = (k*x, k*y)

mag :: Vector2 -> R
mag (x, y) = sqrt $ x*x + y*y
