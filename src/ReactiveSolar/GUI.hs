module ReactiveSolar.GUI
       (createCanvas,
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

import Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk (AttrOp((:=)))
import qualified Graphics.UI.Gtk.OpenGL as GtkGL

import Graphics.Rendering.FTGL as FT

import Graphics.Rendering.OpenGL as GL

import Control.Monad
import Data.IORef
import Control.Applicative

import ReactiveSolar.Data
import ReactiveSolar.Orbit

createCanvas :: IORef SystemState -> IO GtkGL.GLDrawingArea
createCanvas sysState = do
  glconfig <- GtkGL.glConfigNew [GtkGL.GLModeRGBA,
                                 GtkGL.GLModeDepth,
                                 GtkGL.GLModeDouble]

  canvas <- GtkGL.glDrawingAreaNew glconfig
  widgetSetSizeRequest canvas 1024 768

  -- Set the repaint handler
  onExpose canvas $ \_ -> 
    GtkGL.withGLDrawingArea canvas $ \glwindow -> do
      clear [DepthBuffer, ColorBuffer]
      display sysState
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
    perspective 25 (800 / 600) 0.001 500
    matrixMode $= Modelview 0
    loadIdentity
    clearColor $= Color4 0 0 0 0
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
    --polygonSmooth $= Enabled
    --hint PolygonSmooth $= Nicest
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    lineWidth $= 0.25



-- canvasOnExpose :: GtkGL.GLDrawingArea -> IO Bool
-- canvasOnExpose canvas =
--   GtkGL.withGLDrawingArea canvas $ \glwindow -> do
--     clear [DepthBuffer, ColorBuffer]
--     display
--     GtkGL.glDrawableSwapBuffers glwindow
--     return True
    
   
display :: IORef SystemState -> IO ()
display sysState = do
  viewport $= (Position 0 0, Size 1024 768)
  clear [DepthBuffer, ColorBuffer]
  loadIdentity
  c <- readIORef sysState
  let cT = realToFrac $ tilt $ camState c
      cR = realToFrac $ rot $ camState c
      cZ = realToFrac $ zoom $ camState c
      objs = orbits c
  translate (vector3d 0 0 cZ)
  preservingMatrix $ do
    -- renderString "hello solar system!"
    -- the three changes we want to track with IORefs
    rotate cT (vector3d 1 0 0) -- rotate around x axis (tilt)
    rotate cR (vector3d 0 0 1) -- rotate around y axis (rotate)
    -- translate (vector3d 0 0 cZ) -- translate along z axis (zoom)
    drawSunAxis
    drawSun
    mapM_ drawOrbit objs
    
    return ()
  

-- generalize later
drawSun :: IO ()
drawSun = preservingMatrix $ do
  let sunRadius = 0.0046491
  color (Color3 1 1 0 :: Color3 GLfloat)
  materialEmission GL.Front $= (Color4 1 1 0 1 :: Color4 GLfloat)
  renderQuadric (QuadricStyle Nothing NoTextureCoordinates Outside FillStyle) (Sphere sunRadius 100 100)
  return ()

-- drawPlanet :: Planet -> IO ()
-- drawPlanet = 

drawSunAxis :: IO ()
drawSunAxis = renderPrimitive Lines $ do
    color (Color3 0 0 1 :: Color3 GLfloat)
    vertex (vertex3d 0 100 0)
    vertex (vertex3d 0 (-100) 0)
    color (Color3 1 1 1 :: Color3 GLfloat)
    vertex (vertex3d 100 0 0)
    vertex (vertex3d (-100) 0 0)
    color (Color3 0 1 0 :: Color3 GLfloat)
    vertex (vertex3d 0 0 100)
    vertex (vertex3d 0 0 (-100))

-- drawAxis :: Planet -> IO ()
-- drawAxis

drawOrbit :: Orbit -> IO ()
drawOrbit orbit = renderPrimitive LineStrip $ do
  color (Color3 1 1 1 :: Color3 GLfloat)
  let e = elements orbit
      m = map (calcHelioCoords
               (semiMajorAxis e)
               (ecc e)
               (longAscNode e)
               (argPeri e)
               (incl e)) [0..360]
  mapM_ (\pt -> vertex (vertex3d (realToFrac $ xCoord pt)
                                (realToFrac $ yCoord pt)
                                (realToFrac $ zCoord pt))) m
  return ()
    
  

updateCam :: IORef SystemState -> SpinButton -> SpinButton -> SpinButton -> IO ()
updateCam sysState x y z = do
    valx <- liftM realToFrac $ spinButtonGetValue x
    valy <- liftM realToFrac $ spinButtonGetValue y
    valz <- liftM realToFrac $ spinButtonGetValue z
    oldState <- readIORef sysState
    let o = orbits oldState
    let c = CameraState valx valy valz
    writeIORef sysState $ SystemState c o
    return ()

buttonStartAct :: IO ()
buttonStartAct = do
  putStrLn "start"
  return ()

buttonStopAct :: IO ()
buttonStopAct = return ()

buttonListAct :: IO ()
buttonListAct = return ()

buttonAddAct :: IO ()
buttonAddAct = return ()

buttonRemAct :: IO ()
buttonRemAct = return ()

buttonQuitAct :: Window -> IO ()
buttonQuitAct window = do
  -- cleanup code goes here (saving changes)
  widgetDestroy window
  return ()

buttonResetAct :: IORef SystemState -> SpinButton -> SpinButton -> SpinButton -> IO ()
buttonResetAct sysState x y z = do
  spinButtonSetValue x 0
  spinButtonSetValue y 0
  spinButtonSetValue z (-150.0)
  updateCam sysState x y z
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

renderString :: String -> IO ()
renderString str = preservingMatrix $ do
  scale 0.1 0.1 (0.1 :: GLfloat)
  font <- FT.createTextureFont "../data/Inconsolata.otf"
  -- font <- FT.createBufferFont "../data/Inconsolata.otf"
  FT.setFontFaceSize font 18 120
  FT.renderFont font str FT.Front
