{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module FruRein
  (
    fruRein
  , FruRein
  ) where

import Control.Monad.Random
import qualified Data.Map.Strict as M
import Graph (vertices, Graph, Vertex(..), neighbors)
import Tutte (ForceAlgo(..))


type Map = M.Map
type R = Float
type Vector2 = (R, R)


data FruRein =
    FruRein
    { graph :: Graph
    , w, l  :: R
    , k, t  :: R
    , verts :: Map Vertex Vector2
    } deriving (Show)

instance ForceAlgo FruRein where
    positions = verts
    advance = advanceFruRein

fruRein :: Graph -> FruRein
fruRein graph =
    let
        randPosition :: RandomGen g => Rand g Vector2
        randPosition = do
          a <- getRandomR (0.0, 1.0)
          b <- getRandomR (0.0, 1.0)
          return (a, b)

        randPositions :: [Vector2]
        randPositions = evalRand (sequence $ repeat randPosition) (mkStdGen 0)

        verts = M.fromList $ zip vs randPositions
    in FruRein{graph, w, l, k, t, verts}
  where w = 2.0
        l = w
        vs = vertices graph
        numV = length vs
        k = sqrt $ w * l / (fromIntegral numV)
        t = 1.0


advanceFruRein :: FruRein -> FruRein
advanceFruRein f@FruRein{..} =
    let
        disp   = M.fromList $ zip (vertices graph) $ repeat (0.0, 0.0)
        verts' = applyDisp f . repulse f . attract f $ disp
        t' = t + 1
    in f{t = t', verts = verts'}


type DispMap = Map Vertex Vector2
type PosMap = Map Vertex Vector2

attract :: FruRein -> DispMap -> DispMap
attract FruRein{..} disp =
    let
        vdisp :: Vertex -> Vector2 -> Vector2
        vdisp v oldDisp = foldr (f v) oldDisp $ vertices graph

        f :: Vertex -> Vertex -> Vector2 -> Vector2
        f v u vd = change verts v u vd attrForce plus

        attrForce x = k * k / x

    in M.mapWithKey vdisp disp


repulse :: FruRein -> DispMap -> DispMap
repulse FruRein{..} disp =
    let
        vdisp :: Vertex -> Vector2 -> Vector2
        vdisp v oldDisp = foldr (f v) oldDisp $ neighbors graph v

        f :: Vertex -> Vertex -> Vector2 -> Vector2
        f v u vd = change verts v u vd repulseForce op
          where op = if unVert v < unVert u then plus else minus

        repulseForce x = x * x / k

    in M.mapWithKey vdisp disp


applyDisp :: FruRein -> DispMap -> PosMap
applyDisp FruRein{..} disp =
    let
        temp = 0.2 * w / t

        update :: Vertex -> Vector2 -> Vector2
        update v pos = let vd   = disp M.! v
                           dist = mag vd
                           (px, py) = plus pos $
                                          scale vd (min dist temp / dist)
                           px' = (w / 2.0) `min` ((negate w / 2.0) `max` px)
                           py' = (l / 2.0) `min` ((negate l / 2.0) `max` py)
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

change verts v u vd force op|v==u = vd
                            |otherwise =
    let vp    = verts M.! v
        up    = verts M.! u
        delta = minus vp up
        dist   = mag delta
    in vd `op` (scale delta $ force dist / dist)

