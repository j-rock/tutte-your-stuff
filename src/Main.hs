{-# LANGUAGE NamedFieldPuns #-}

module Main
  (
    main
  ) where

import Control.Arrow ((***))
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Control.Monad (forM_)
import Data.Char (ord)

import Graphics.UI.GLUT

import Graph
import Tutte
import OpenGLApp

main :: IO ()
main = do
  tAppData <- atomically $ newTVar basicAppData
  openGLMain $ GLApp
    { title = "Tutte's Algorithm"
    , display = appDisplay tAppData
    , special = appSpecial tAppData
    , keyboard = appKeyboard tAppData
    }


data AppData =
    AppData
    { graphs :: [Graph]
    , tutte  :: Tutte
    , theta :: GLdouble
    , go :: Bool
    }

buildAppData :: [Graph] -> AppData
buildAppData graphs = AppData{go = False, graphs, tutte = startTutte $ head graphs, theta = 3.1415926 * 1.5}

basicAppData :: AppData
basicAppData = buildAppData
    [ completeGraph 4
    , hubGraph 7
    , hubGraph 50
    , g
    ]
  where Just g = mkGraph 6 [(0,1),(1,2),(2,3),(3,4),(4,0),(3,5),(1,5)]




type GraphChangeFunc = [Graph] -> [Graph]

appSpecial :: TVar AppData -> SpecialCallback
appSpecial tAppData key _ =
    let
        switcher :: GraphChangeFunc -> IO ()
        switcher graphChange = atomically $ do
            AppData{graphs, theta} <- readTVar tAppData
            let app' = buildAppData $ graphChange graphs
            writeTVar tAppData app'{theta}

        switchGraphLeft :: IO ()
        switchGraphLeft = switcher (\gs -> last gs : init gs)

        switchGraphRight :: IO ()
        switchGraphRight = switcher (\gs -> tail gs ++ [head gs])

    in case key of
         KeyLeft  -> switchGraphLeft
         KeyRight -> switchGraphRight
         _        -> return ()

appDisplay :: TVar AppData -> DisplayCallback
appDisplay tAppData =
    let
        drawVertyBits :: Graph -> Tutte -> Graph.Vertex -> IO ()
        drawVertyBits g t v = do
          let vPos = t ! v
          drawNode vPos
          forM_ (neighbors g v) $ \w -> do
            if unVert w < unVert v
            then drawEdge vPos (t ! w)
            else return ()

        center = Vertex3 0.0 0.0 0.0
        up     = Vector3 0.0 1.0 0.0

        eye :: GLdouble -> Vertex3 GLdouble
        eye theta = Vertex3 x 0 z
          where x = 3 * cos theta
                z = 3 * sin theta

    in     do
       a@AppData{go, tutte, theta} <- atomically $ readTVar tAppData
       lookAt (eye theta) center up
       let graph = getGraph tutte
           verts = vertices graph
       forM_ verts $ drawVertyBits graph tutte
       if go then atomically $ writeTVar tAppData a{tutte = advanceTutte tutte}
             else return ()

convertPos :: (Float, Float) -> (GLfloat, GLfloat)
convertPos = realToFrac *** realToFrac

drawNode :: (Float, Float) -> IO ()
drawNode pos = preservingMatrix $ do
  let (x, y) = convertPos pos
  color (Color3 1.0 0.0 0.0 :: Color3 GLfloat)
  translate $ Vector3 x y 0.0
  renderObject Solid $ Sphere' 0.1 6 6

drawEdge :: (Float, Float) -> (Float, Float) -> IO ()
drawEdge s t = renderPrimitive Lines $ do
  let ((sx, sy), (tx, ty)) = (convertPos *** convertPos) (s, t)
  color (Color3 0.0 1.0 0.0 :: Color3 GLfloat)
  vertex $ Vertex3 sx sy 0.0
  vertex $ Vertex3 tx ty 0.0


appKeyboard :: TVar AppData -> KeyboardCallback
appKeyboard tAppData key _ =
    let
        changeTheta :: GLdouble -> IO ()
        changeTheta dtheta = atomically $ do
            a@AppData{theta} <- readTVar tAppData
            writeTVar tAppData a{theta = theta + dtheta}

        stepper :: IO ()
        stepper = atomically $ do
          a@AppData{tutte} <- readTVar tAppData
          writeTVar tAppData a{tutte = advanceTutte tutte}

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
