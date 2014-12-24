{-# LANGUAGE NamedFieldPuns #-}

module Graph
  (
    Vertex(..)
  , Adjacency
  , Graph
  , adj
  , Edge
  , mkGraph
  , neighbors
  , vertices
  , completeGraph
  , hubGraph
  , dfs
  , findBestCycle
  )

  where

import Control.Arrow ((&&&))
import Control.Monad (guard)
import Data.List (partition, maximumBy)
import Data.Ord (comparing)
import qualified Data.Set as S
import qualified Data.Map.Strict as M

type Set = S.Set
type Map = M.Map


newtype Vertex = Vertex {unVert :: Int} deriving (Eq, Ord, Show)
type Adjacency = Map Vertex (Set Vertex)
data Graph = Graph {adj :: Adjacency} deriving (Eq, Ord, Show)


-- For an edge (a,b), the implication is that the
-- Vertex indexed by a and the Vertex indexed by b
-- have an undirected edge between them.
type Edge = (Int, Int)



mkGraph :: Int     -- the number of vertices
        -> [Edge]  -- the associated edges
        -> Maybe Graph
mkGraph numV edges = do
    guard $ numV > 0
    guard $ all withinBounds edges
    return $ Graph $ makeAdj numV edges
  where bnded x = x >= 0 && x < numV
        withinBounds (x, y) = bnded x && bnded y

makeAdj :: Int -> [Edge] -> Adjacency
makeAdj numV edges =
    let baseAdj = M.fromList $ map (Vertex &&& const S.empty) [0..numV-1]
    in foldr f baseAdj edges
  where f :: Edge -> Adjacency -> Adjacency
        f (v0, v1) = let v0' = Vertex v0
                         v1' = Vertex v1
                     in add v0' v1' . add v1' v0'
        add u v = M.insertWith S.union u $ S.singleton v

vertices :: Graph -> [Vertex]
vertices Graph{adj} = M.keys adj

neighbors :: Graph -> Vertex -> [Vertex]
neighbors Graph{adj} v = S.toList $ adj M.! v

completeGraph :: Int -> Graph
completeGraph n = Graph $ makeAdj n complete'
  where complete' = [(u,v) | u <- [0..n-2], v <- [u..n-1], u /= v]

circleGraph :: Int -> Graph
circleGraph n = Graph $ makeAdj n $ circleEdges n

circleEdges n = (0, n-1) : [(u,u-1) | u <- [1..n-1]]

hubGraph :: Int -> Graph
hubGraph n = Graph $ makeAdj (n+1) $ circleEdges n ++ hub'
  where hub' = [(u,n) | u <- [0..n-1]]



type Visited  = Set Vertex
type VPred    = Maybe Vertex
type PredMap  = Map Vertex VPred
type BackEdge = Edge
type DFSInfo  = (Visited, PredMap, [BackEdge])

dfs :: Graph -> (PredMap, [BackEdge])
dfs g@Graph{adj} =
    let (_, p, e) = go Nothing (Vertex 0) startInfo
    in (p, e)
  where startInfo = (S.empty, M.empty, [])
        go :: VPred -> Vertex -> DFSInfo -> DFSInfo
        go p v (vis,pred,backs)|S.member v vis = (vis, pred, backs)
                               |otherwise =
          let vcs   = filter (\c -> Just c /= p) $ neighbors g v
              (backVs, unvisited) = partition (\c -> S.member c vis) vcs
              vvis  = S.insert v vis
              vpred = M.insert v p pred
              vbcks = map (const (unVert v) &&& unVert) backVs
          in foldr (go (Just v)) (vvis, vpred, backs ++ vbcks) unvisited


type Cycle = [Vertex]

cycles :: Graph -> [Cycle]
cycles g =
    let retrieveCycle :: PredMap -> Edge -> Cycle
        retrieveCycle preds (a,b) = buildPath vb va []
          where (va, vb) = (Vertex a, Vertex b)
                buildPath u v path|u == v = u:path
                                  |otherwise =
                    case preds M.! v of
                      Nothing -> path
                      Just w  -> buildPath u w (v:path)
        (preds, backs) = dfs g
    in  map (retrieveCycle preds) backs

findBestCycle :: Graph -> Cycle
findBestCycle g@Graph{adj} =
    case cycles g of
      []  -> []
      [c] -> c
      cs  -> head cs
    --   cs  -> let cs' = filter ((< M.size adj) . length) cs
    --          in if length cs' > 0
    --             then maximumBy (comparing length) cs'
    --             else head cs
