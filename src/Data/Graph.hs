{-# LANGUAGE NamedFieldPuns #-}

module Data.Graph
  (
    Vertex(..)
  , Adjacency
  , Graph
  , adj
  , Edge
  , mkGraph
  , children
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


newtype Vertex = Vertex {unVert :: Int} deriving (Eq, Ord, Show)
type Adjacency = M.Map Vertex (S.Set Vertex)
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

completeGraph :: Int -> Graph
completeGraph n = Graph $ makeAdj n complete'
  where complete' = [(u,v) | u <- [0..n-2], v <- [u..n-1], u /= v]

circleGraph :: Int -> Graph
circleGraph n = Graph $ makeAdj n $ circleEdges n

circleEdges n = (0, n-1) : [(u,u-1) | u <- [1..n-1]]

hubGraph :: Int -> Graph
hubGraph n = Graph $ makeAdj (n+1) $ circleEdges n ++ hub'
  where hub' = [(u,n) | u <- [0..n-1]]

children :: Graph -> Vertex -> [Vertex]
children Graph{adj} v = S.toList $ adj M.! v


type Visited  = S.Set Vertex
type VPred    = Maybe Vertex
type Pred     = M.Map Vertex VPred
type BackEdge = Edge
type DFSInfo  = (Visited, Pred, [BackEdge])

dfs :: Graph -> (Pred, [BackEdge])
dfs g@Graph{adj} =
    let (_, p, e) = go startInfo Nothing $ Vertex 0
    in (p, e)
  where startInfo = (S.empty, M.empty, [])
        go :: DFSInfo -> VPred -> Vertex -> DFSInfo
        go (vis,pred,backs) p v|S.member v vis = (vis, pred, backs)
                               |otherwise =
          let vcs   = filter (\c -> Just c /= p) $ children g v
              (backVs, unvisited) = partition (\c -> S.member c vis) vcs
              vvis  = S.insert v vis
              vpred = M.insert v p pred
              vbcks = map (const (unVert v) &&& unVert) backVs
          in foldr (go' (Just v)) (vvis, vpred, backs ++ vbcks) unvisited
        go' :: VPred -> Vertex -> DFSInfo -> DFSInfo
        go' p v info = go info p v

findBestCycle :: Graph -> Maybe [Vertex]
findBestCycle g = let (preds, backs) = dfs g
                      backEdgePaths = map (convertPath preds) backs
                  in safeMax $ map (length &&& id) $ backEdgePaths
  where safeMax [] = Nothing
        safeMax xs = Just $ snd $ maximumBy (comparing fst) xs
        convertPath preds (a, b) =
            let va = Vertex a; vb = Vertex b
            in buildPath preds vb va []
        buildPath preds vb va nodes| va == vb = (vb:nodes)
                                   | otherwise =
            case preds M.! va of
              Nothing -> nodes
              Just vc -> buildPath preds vb vc (va:nodes)
