{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}


module Main
  (
    main
  ) where

import Control.Applicative ((<$>))
import Control.Arrow ((***))
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Control.Monad (forM_)
import Data.Map.Strict (Map, (!), keys)

import Graphics.UI.GLUT

import Graph
import Tutte
import FruRein
import Walshaw
import OpenGLApp

main :: IO ()
main = do
  tAppData <- atomically . newTVar =<< buildAppData <$> randomGraphs
  openGLMain $ GLApp
    { title = "Graph Planarization"
    , display = appDisplay tAppData
    , special = appSpecial tAppData
    , keyboard = appKeyboard tAppData
    }

data Zipper a = Zipper [a] a [a]
fromList :: [a] -> Zipper a
fromList []     = error "Bad list passed to zipper"
fromList (x:xs) = Zipper [] x xs

pick :: Zipper a -> a
pick (Zipper _ x _) = x

left :: Zipper a -> Zipper a
left z@(Zipper [] _ _)    = z
left (Zipper (x:xs) y ys) = Zipper xs x (y:ys)

right :: Zipper a -> Zipper a
right z@(Zipper _ _ [])    = z
right (Zipper ys y (x:xs)) = Zipper (y:ys) x xs

data AppData a =
    AppData
    { graphs :: Zipper Graph
    , algos  :: ForceAlgo a => Zipper Algo
    , algo   :: ForceAlgo a => a
    , theta  :: GLdouble
    , go     :: Bool
    }

type TApp a = TVar (AppData a)

graph :: AppData a -> Graph
graph = pick . graphs

verts :: ForceAlgo a => AppData a -> Map Graph.Vertex (Float, Float)
verts AppData{..} = positions algo

advanceApp :: ForceAlgo a => TApp a -> STM ()
advanceApp tApp = do
    a@AppData{..} <- readTVar tApp
    writeTVar tApp a{algo = advance algo}


data Algo = forall a. ForceAlgo a => AlgoC (Graph -> a)
          | forall a. ForceAlgo a => Algo a

instance ForceAlgo Algo where
    positions (Algo x) = positions x
    positions _        = error "Can't use AlgoC for positions"
    advance   (Algo x) = Algo $ advance x
    advance   _        = error "Can't use AlgoC for advance"

algoApp :: Algo -> Graph -> Algo
algoApp (AlgoC f) g = Algo (f g)
algoApp _         _ = error "Can't use Algo for construction"

buildAppData :: [Graph] -> AppData Algo
buildAppData graphs = buildAppData' (fromList graphs) (fromList allAlgos)
  where allAlgos = [AlgoC tutte, AlgoC fruRein, AlgoC walshaw]

buildAppData' :: Zipper Graph -> Zipper Algo -> AppData Algo
buildAppData' graphs algos =
    AppData
    { graphs
    , algos
    , algo = (pick algos) `algoApp` (pick graphs)
    , theta = 3.1415926 * 1.5
    , go = False
    }

appSpecial :: TApp Algo -> SpecialCallback
appSpecial tAppData key _ =
    let
        switcher :: (Zipper Graph -> Zipper Graph) -> IO ()
        switcher graphChange = atomically $ do
            AppData{..} <- readTVar tAppData
            let app' = buildAppData' (graphChange graphs) algos
            writeTVar tAppData app'{theta}

        switcher' :: (Zipper Algo -> Zipper Algo) -> IO ()
        switcher' algoChange = atomically $ do
            AppData{..} <- readTVar tAppData
            let app' = buildAppData' graphs $ algoChange algos
            writeTVar tAppData app'{theta}

    in case key of
         KeyLeft  -> switcher left
         KeyRight -> switcher right
         KeyUp    -> switcher' left
         KeyDown  -> switcher' right
         _        -> return ()

appDisplay :: TApp Algo -> DisplayCallback
appDisplay tAppData =
    let
        drawVertyBits :: Graph -> Map Graph.Vertex (Float, Float) -> IO ()
        drawVertyBits g m = forM_ (keys m) $ \v -> do
          let vPos = m ! v
          drawNode vPos
          forM_ (neighbors g v) $ \w -> do
            if unVert w < unVert v
            then drawEdge vPos (m ! w)
            else return ()

        center = Vertex3 0.0 0.0 0.0
        up     = Vector3 0.0 1.0 0.0

        eye :: GLdouble -> Vertex3 GLdouble
        eye theta = Vertex3 x 0 z
          where x = 3 * cos theta
                z = 3 * sin theta

    in     do
       a@AppData{..} <- atomically $ readTVar tAppData
       lookAt (eye theta) center up
       drawVertyBits (Main.graph a) (verts a)
       if go then atomically $ advanceApp tAppData
             else return ()


convertPos :: (Float, Float) -> (GLfloat, GLfloat)
convertPos = realToFrac *** realToFrac

drawNode :: (Float, Float) -> IO ()
drawNode pos = preservingMatrix $ do
  let (x, y) = convertPos pos
  color (Color3 0.839 0.117 0.323 :: Color3 GLfloat)
  translate $ Vector3 x y 0.0
  renderObject Solid $ Sphere' 0.03 8 8

drawEdge :: (Float, Float) -> (Float, Float) -> IO ()
drawEdge s t = renderPrimitive Lines $ do
  let ((sx, sy), (tx, ty)) = (convertPos *** convertPos) (s, t)
  color (Color3 0.6 0.0 0.8 :: Color3 GLfloat)
  vertex $ Vertex3 sx sy 0.0
  vertex $ Vertex3 tx ty 0.0


appKeyboard :: TApp Algo -> KeyboardCallback
appKeyboard tAppData key _ =
    let
        changeTheta :: GLdouble -> IO ()
        changeTheta dtheta = atomically $ do
            a@AppData{theta} <- readTVar tAppData
            writeTVar tAppData a{theta = theta + dtheta}

        stepper :: IO ()
        stepper = atomically $ advanceApp tAppData

        setGo :: IO ()
        setGo = atomically $ do
          appData <- readTVar tAppData
          writeTVar tAppData appData{go = True}

    in case key of
         'a'  -> changeTheta (-dt)
         'd'  -> changeTheta dt
         ' '  -> stepper
         '\r' -> setGo
         _    -> return ()
      where dt = 0.1
