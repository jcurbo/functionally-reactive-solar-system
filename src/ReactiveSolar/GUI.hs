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
        buttonResetAct,
        updateScaleFromSpinner,
        buttonScaleDayAct,
        buttonScaleYearAct
        ) where

import Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk (AttrOp((:=)))
import qualified Graphics.UI.Gtk.OpenGL as GtkGL

import Graphics.Rendering.FTGL as FT

import Graphics.Rendering.OpenGL as GL

import Control.Monad
import Data.IORef

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
    rotate cT (vector3d 1 0 0)
    rotate cR (vector3d 0 0 1)
    drawSunAxis
    drawSun
    mapM_ drawOrbit objs
    mapM_ drawPlanetRadius objs
    mapM_ drawPlanet objs
    
    return ()
  
drawSun :: IO ()
drawSun = preservingMatrix $ do
  let sunRadius = 0.0046491
  color (Color3 1 1 0 :: Color3 GLfloat)
  materialEmission GL.Front $= (Color4 1 1 0 1 :: Color4 GLfloat)
  renderQuadric (QuadricStyle Nothing NoTextureCoordinates Outside FillStyle) (Sphere sunRadius 100 100)
  return ()

drawPlanet :: Orbit -> IO ()
drawPlanet orbit = preservingMatrix $ do
  color (Color3 0 0 1 :: Color3 GLfloat)
  let e = elements orbit
      c = calcHelioCoords
            (semiMajorAxis e)
            (ecc e)
            (longAscNode e)
            (argPeri e)
            (incl e)
            (curTrueAnomaly orbit)
  translate (vector3d (realToFrac $ xCoord c)
                      (realToFrac $ yCoord c)
                      (realToFrac $ zCoord c))
  materialEmission GL.Front $= (Color4 0 0 1 1 :: Color4 GLfloat)
  renderQuadric (QuadricStyle Nothing NoTextureCoordinates Outside FillStyle) (Sphere 0.001 100 100)
  if (distPeri e > 4.0)
    then renderString (name e) 0.1
    else renderString (name e ) 0.01

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
    
drawPlanetRadius :: Orbit -> IO ()
drawPlanetRadius orbit = renderPrimitive Lines $ do
  color (Color3 0 1 0 :: Color3 GLfloat)
  let e = elements orbit
      c = calcHelioCoords
            (semiMajorAxis e)
            (ecc e)
            (longAscNode e)
            (argPeri e)
            (incl e)
            (curTrueAnomaly orbit)
  vertex (vertex3d 0 0 0)
  vertex (vertex3d (realToFrac $ xCoord c)
                   (realToFrac $ yCoord c)
                   (realToFrac $ zCoord c))

updateCam :: IORef SystemState -> SpinButton -> SpinButton -> SpinButton -> IO ()
updateCam sysState x y z = do
    valx <- liftM realToFrac $ spinButtonGetValue x
    valy <- liftM realToFrac $ spinButtonGetValue y
    valz <- liftM realToFrac $ spinButtonGetValue z
    oldState <- readIORef sysState
    let o = orbits oldState
        c = CameraState valx valy valz
        s = scalefac oldState
        d = delayTime oldState
    writeIORef sysState $ SystemState c o s d
    return ()

updateScaleFromSpinner :: IORef SystemState -> SpinButton -> IO ()
updateScaleFromSpinner sysState s = do
  vals <- liftM truncate $ spinButtonGetValue s
  updateScale sysState vals

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

buttonScaleAct :: IORef SystemState -> SpinButton -> Int -> IO ()
buttonScaleAct sysState s val = do
  spinButtonSetValue s (fromIntegral val)
  updateScale sysState val

buttonScaleDayAct :: IORef SystemState -> SpinButton -> IO ()
buttonScaleDayAct sysState s = do
  buttonScaleAct sysState s 86400

buttonScaleYearAct :: IORef SystemState -> SpinButton -> IO ()
buttonScaleYearAct sysState s = do
  buttonScaleAct sysState s (86400 * 365)

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

renderString :: String -> Double -> IO ()
renderString str scalef = preservingMatrix $ do
  let scalef' = realToFrac scalef
  scale scalef' scalef' (scalef' :: GLfloat)
  font <- FT.createTextureFont "../data/Inconsolata.otf"
  FT.setFontFaceSize font 18 120
  FT.renderFont font str FT.Front
