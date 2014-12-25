{-# LANGUAGE RecordWildCards #-}

module OpenGLApp
  (
    openGLMain
  , GLApp(..)
  ) where

import Graphics.UI.GLUT

data GLApp =
    GLApp
    { title :: String
    , display :: DisplayCallback
    , special :: SpecialCallback
    , keyboard :: KeyboardCallback
    }

openGLMain :: GLApp -> IO ()
openGLMain GLApp{..} = do
    _ <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered, RGBMode, WithDepthBuffer]
    window <- createWindow title
    windowSize $= Size 540 400
    clearColor $= Color4 0.05 0.05 0.05 0.0
    clearDepth $= 1
    shadeModel $= Smooth
    depthFunc $= Just Less
    hint PerspectiveCorrection $= Nicest
    reshapeCallback $= Just reshape
    specialCallback $= Just special
    keyboardCallback $= Just keyboard
    displayCallback $= wrapDisplay display window
    mainLoop


reshape :: ReshapeCallback
reshape s@(Size width height) = do
    viewport $= (Position 0 0, s)
    matrixMode $= Projection
    loadIdentity
    perspective 45 (fromIntegral width / fromIntegral height) 2.01 4
    matrixMode $= Modelview 0

wrapDisplay :: DisplayCallback -> Window -> DisplayCallback
wrapDisplay display window = do
    clear [ColorBuffer, DepthBuffer]
    loadIdentity
    display
    flush
    swapBuffers
    postRedisplay (Just window)
