module Main (main) where
 
import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk (AttrOp((:=)))
import qualified Graphics.UI.Gtk.OpenGL as GtkGL
 
import Graphics.Rendering.OpenGL as GL
 
main :: IO ()
main = do 
  Gtk.initGUI
 
  -- Initialise the Gtk+ OpenGL extension
  -- (including reading various command line parameters)
  GtkGL.initGL
 
  -- We need a OpenGL frame buffer configuration to be able to create other
  -- OpenGL objects.
  glconfig <- GtkGL.glConfigNew [GtkGL.GLModeRGBA,
                                 GtkGL.GLModeDepth,
                                 GtkGL.GLModeDouble]
 
  -- Create an OpenGL drawing area widget
  canvas <- GtkGL.glDrawingAreaNew glconfig
 
  Gtk.widgetSetSizeRequest canvas 250 250
 
  -- Initialise some GL setting just before the canvas first gets shown
  -- (We can't initialise these things earlier since the GL resources that
  -- we are using wouldn't heve been setup yet)
  Gtk.onRealize canvas $ GtkGL.withGLDrawingArea canvas $ \_ -> do
    clearColor $= (Color4 0.0 0.0 0.0 0.0)
    matrixMode $= Projection
    loadIdentity
    ortho 0.0 1.0 0.0 1.0 (-1.0) 1.0
    depthFunc $= Just Less
    drawBuffer $= BackBuffers
 
  -- Set the repaint handler
  Gtk.onExpose canvas $ \_ -> do
    GtkGL.withGLDrawingArea canvas $ \glwindow -> do
      GL.clear [GL.DepthBuffer, GL.ColorBuffer]
      display
      GtkGL.glDrawableSwapBuffers glwindow
    return True
 
  -- Setup the animation
  Gtk.timeoutAddFull (do
      Gtk.widgetQueueDraw canvas
      return True)
    Gtk.priorityDefaultIdle animationWaitTime
 
  --------------------------------
  -- Setup the rest of the GUI:
  --
  window <- Gtk.windowNew
  Gtk.onDestroy window Gtk.mainQuit
  Gtk.set window [ Gtk.containerBorderWidth := 8,
                   Gtk.windowTitle := "Gtk2Hs + HOpenGL demo" ]
 
  vbox <- Gtk.vBoxNew False 4
  Gtk.set window [ Gtk.containerChild := vbox ]
 
  label <- Gtk.labelNew (Just "Gtk2Hs using OpenGL via HOpenGL!")
  button <- Gtk.buttonNewWithLabel "Close"
  Gtk.onClicked button $ Gtk.widgetDestroy window
  Gtk.set vbox [ Gtk.containerChild := canvas,
                 Gtk.containerChild := label,
                 Gtk.containerChild := button ]
 
  Gtk.widgetShowAll window
  Gtk.mainGUI
 
-- Draw the OpenGL polygon.
display = do
  loadIdentity
  color (Color3 1 1 1 :: Color3 GLfloat)
  -- Instead of glBegin ... glEnd there is renderPrimitive.  
  renderPrimitive Polygon $ do
    vertex (Vertex3 0.25 0.25 0.0 :: Vertex3 GLfloat)
    vertex (Vertex3 0.75 0.25 0.0 :: Vertex3 GLfloat)
    vertex (Vertex3 0.75 0.75 0.0 :: Vertex3 GLfloat)
    vertex (Vertex3 0.25 0.75 0.0 :: Vertex3 GLfloat)

  preservingMatrix $ do
    rotate 0 (vector3f 1.0 0.0 0.0)
    rotate 0 (vector3f 0.0 1.0 0.0)
    color (Color3 1.0 1.0 0.0 :: Color3 GLfloat)
    materialEmission Front $= (Color4 1.0 1.0 0.0 1.0 :: Color4 GLfloat)
    renderQuadric (QuadricStyle Nothing NoTextureCoordinates Outside FillStyle) (Sphere 1.0 100 100)
    return ()
 
animationWaitTime = 3

vertex3f :: GLfloat -> GLfloat -> GLfloat -> Vertex3 GLfloat
vertex3f = Vertex3

vector3f :: GLfloat -> GLfloat -> GLfloat -> Vector3 GLfloat
vector3f = Vector3
