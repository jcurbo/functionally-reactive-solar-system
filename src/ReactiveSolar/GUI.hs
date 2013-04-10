module ReactiveSolar.GUI
       (CameraState(..),
        createCanvas,
        canvasOnRealize,
        updateCam,
        buttonStartAct,
        buttonStopAct,
        buttonRemAct,
        buttonQuitAct,
        buttonListAct,
        buttonAddAct,
        buttonResetAct
        ) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk (AttrOp((:=)))
import qualified Graphics.UI.Gtk.OpenGL as GtkGL

import Graphics.Rendering.OpenGL

import Control.Monad
import Data.IORef

data CameraState = CameraState { tilt :: Double,
                                 rot :: Double,
                                 zoom :: Double }

createCanvas :: IORef CameraState -> IO GtkGL.GLDrawingArea
createCanvas camState = do
  glconfig <- GtkGL.glConfigNew [GtkGL.GLModeRGBA,
                                 GtkGL.GLModeDepth,
                                 GtkGL.GLModeDouble]

  canvas <- GtkGL.glDrawingAreaNew glconfig
  widgetSetSizeRequest canvas 1024 768

  -- Set the repaint handler
  onExpose canvas $ \_ -> 
    GtkGL.withGLDrawingArea canvas $ \glwindow -> do
      clear [DepthBuffer, ColorBuffer]
      display camState
      GtkGL.glDrawableSwapBuffers glwindow
      return True

  -- Setup the animation
  let animationWaitTime = 3
  timeoutAddFull (do
                     widgetQueueDraw canvas
                     return True)
    priorityDefaultIdle animationWaitTime

  return canvas

canvasOnRealize :: GtkGL.GLDrawingArea -> IO ()
canvasOnRealize canvas = 
  GtkGL.withGLDrawingArea canvas $ \_ -> do
    matrixMode $= Projection
    loadIdentity
    perspective 75 (800 / 600) 0.001 500
    matrixMode $= Modelview 0
    loadIdentity
    clearColor $= (Color4 0 0 0 0)
    -- shadeModel $= Smooth
    -- clearDepth $= 1.0
    -- depthFunc $= Just Lequal
    -- drawBuffer $= BackBuffers
    -- hint PerspectiveCorrection $= Nicest
    -- -- lighting
    -- let l = Light 0
    -- light l $= Enabled
    -- lighting $= Enabled
    -- position l $= vertex4f 0.0 0.0 0.0 1.0
    colorMaterial $= Just (FrontAndBack, Diffuse)
    lineSmooth $= Enabled
    hint LineSmooth $= Nicest
    hint PolygonSmooth $= Nicest


-- canvasOnExpose :: GtkGL.GLDrawingArea -> IO Bool
-- canvasOnExpose canvas =
--   GtkGL.withGLDrawingArea canvas $ \glwindow -> do
--     clear [DepthBuffer, ColorBuffer]
--     display
--     GtkGL.glDrawableSwapBuffers glwindow
--     return True
    
   
display :: IORef CameraState -> IO ()
display camState = do
  clear [DepthBuffer, ColorBuffer]
  loadIdentity
  c <- readIORef camState
  let cT = realToFrac $ tilt c
      cR = realToFrac $ rot c
      cZ = realToFrac $ zoom c
  -- lookAt (c :: Vertex3 GLdouble) (vertex3d 0.0 0.0 0.0) (vector3d 0.0 1.0 0.0)
  translate (vector3d 0 0 cZ)
  preservingMatrix $ do
    -- the three changes we want to track with IORefs
    rotate cT (vector3d 1 0 0) -- rotate around x axis (tilt)
    rotate cR (vector3d 0 1 0) -- rotate around y axis (rotate)
    -- translate (vector3d 0 0 cZ) -- translate along z axis (zoom)
    drawSunAxis
    drawSun
  

-- generalize later
drawSun :: IO ()
drawSun = preservingMatrix $ do
  let sunRadius = 0.0046491
  color (Color3 1 1 0 :: Color3 GLfloat)
  materialEmission Front $= (Color4 1 1 0 1 :: Color4 GLfloat)
  renderQuadric (QuadricStyle Nothing NoTextureCoordinates Outside FillStyle) (Sphere sunRadius 100 100)
  return ()

drawSunAxis :: IO ()
drawSunAxis = renderPrimitive Lines $ do
    color (Color3 0 0 1 :: Color3 GLfloat)
    vertex (vertex3d 0 0.06 0)
    vertex (vertex3d 0 (-0.06) 0)
    color (Color3 1 1 1 :: Color3 GLfloat)
    vertex (vertex3d 0.06 0 0)
    vertex (vertex3d (-0.06) 0 0)
    color (Color3 0 1 0 :: Color3 GLfloat)
    vertex (vertex3d 0 0 0.06)
    vertex (vertex3d 0 0 (-0.06))

updateCam :: IORef CameraState -> SpinButton -> SpinButton -> SpinButton -> IO ()
updateCam camState x y z = do
    valx <- liftM realToFrac $ spinButtonGetValue x
    valy <- liftM realToFrac $ spinButtonGetValue y
    valz <- liftM realToFrac $ spinButtonGetValue z
    let c = CameraState valx valy valz
    writeIORef camState c
    return ()

buttonStartAct :: IO ()
buttonStartAct = do
  putStrLn "start"
  return ()

buttonStopAct :: IO ()
buttonStopAct = do return ()

buttonListAct :: IO ()
buttonListAct = do return ()

buttonAddAct :: IO ()
buttonAddAct = do return ()

buttonRemAct :: IO ()
buttonRemAct = do return ()

buttonQuitAct :: Window -> IO ()
buttonQuitAct window = do
  -- cleanup code goes here (saving changes)
  widgetDestroy window
  return ()

buttonResetAct :: IORef CameraState -> SpinButton -> SpinButton -> SpinButton -> IO ()
buttonResetAct camState x y z = do
  spinButtonSetValue x 30
  spinButtonSetValue y 0
  spinButtonSetValue z (-0.5)
  updateCam camState x y z
  return ()

-- OpenGL helpers
vertex3f :: GLfloat -> GLfloat -> GLfloat -> Vertex3 GLfloat
vertex3f = Vertex3

vertex3d :: GLdouble -> GLdouble -> GLdouble -> Vertex3 GLdouble
vertex3d = Vertex3

vertex4f :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> Vertex4 GLfloat
vertex4f = Vertex4

vertex4d :: GLdouble -> GLdouble -> GLdouble -> GLdouble -> Vertex4 GLdouble
vertex4d = Vertex4

vector3f :: GLfloat -> GLfloat -> GLfloat -> Vector3 GLfloat
vector3f = Vector3

vector3d :: GLdouble -> GLdouble -> GLdouble -> Vector3 GLdouble
vector3d = Vector3


