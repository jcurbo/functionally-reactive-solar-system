module ReactiveSolar.GUI where

import Graphics.UI.Gtk
import Graphics.UI.Gtk (AttrOp((:=)))
import qualified Graphics.UI.Gtk.OpenGL as GtkGL

import Graphics.Rendering.OpenGL



-- menu code, modified from Gtk2hs/gtk/demo/ActionMenu.hs

-- uiDef =
--   "<ui>\
-- \ <menubar>\
-- \ <menu name=\"File\" action=\"FileAction\">\
-- \ <menuitem name=\"New\" action=\"NewAction\" />\
-- \ <menuitem name=\"Open\" action=\"OpenAction\" />\
-- \ <menuitem name=\"Save\" action=\"SaveAction\" />\
-- \ <menuitem name=\"SaveAs\" action=\"SaveAsAction\" />\
-- \ <separator/>\
-- \ <menuitem name=\"Exit\" action=\"ExitAction\"/>\
-- \ <placeholder name=\"FileMenuAdditions\" />\
-- \ </menu>\
-- \ <menu name=\"Edit\" action=\"EditAction\">\
-- \ <menuitem name=\"Cut\" action=\"CutAction\"/>\
-- \ <menuitem name=\"Copy\" action=\"CopyAction\"/>\
-- \ <menuitem name=\"Paste\" action=\"PasteAction\"/>\
-- \ </menu>\
-- \ </menubar>\
-- \ <toolbar>\
-- \ <placeholder name=\"FileToolItems\">\
-- \ <separator/>\
-- \ <toolitem name=\"New\" action=\"NewAction\"/>\
-- \ <toolitem name=\"Open\" action=\"OpenAction\"/>\
-- \ <toolitem name=\"Save\" action=\"SaveAction\"/>\
-- \ <separator/>\
-- \ </placeholder>\
-- \ <placeholder name=\"EditToolItems\">\
-- \ <separator/>\
-- \ <toolitem name=\"Cut\" action=\"CutAction\"/>\
-- \ <toolitem name=\"Copy\" action=\"CopyAction\"/>\
-- \ <toolitem name=\"Paste\" action=\"PasteAction\"/>\
-- \ <separator/>\
-- \ </placeholder>\
-- \ </toolbar>\
-- \</ui>"

-- createMenu :: IO UIManager
-- createMenu = do
--   fileAct <- actionNew "FileAction" "File" Nothing Nothing
--   editAct <- actionNew "EditAction" "Edit" Nothing Nothing

--   -- Create menu items
--   newAct <- actionNew "NewAction" "New"
--             (Just "Clear the spreadsheet area.")
--             (Just stockNew)
--   openAct <- actionNew "OpenAction" "Open"
--              (Just "Open an existing spreadsheet.")
--              (Just stockOpen)
--   saveAct <- actionNew "SaveAction" "Save"
--              (Just "Save the current spreadsheet.")
--              (Just stockSave)
--   saveAsAct <- actionNew "SaveAsAction" "SaveAs"
--                (Just "Save spreadsheet under new name.")
--                (Just stockSaveAs)
--   exitAct <- actionNew "ExitAction" "Exit"
--              (Just "Exit this application.")
--              (Just stockSaveAs)
--   cutAct <- actionNew "CutAction" "Cut"
--             (Just "Cut out the current selection.")
--             (Just stockCut)
--   copyAct <- actionNew "CopyAction" "Copy"
--              (Just "Copy the current selection.")
--              (Just stockCopy)
--   pasteAct <- actionNew "PasteAction" "Paste"
--               (Just "Paste the current selection.")
--               (Just stockPaste)
  
--   standardGroup <- actionGroupNew "standard"
--   mapM_ (actionGroupAddAction standardGroup) [fileAct, editAct]
--   mapM_ (\act -> actionGroupAddActionWithAccel standardGroup act Nothing)
--     [newAct, openAct, saveAct, saveAsAct, exitAct, cutAct, copyAct, pasteAct]
  
--   ui <- uiManagerNew
--   mid <- uiManagerAddUiFromString ui uiDef
--   uiManagerInsertActionGroup ui standardGroup 0

--   return ui

createCanvas :: IO GtkGL.GLDrawingArea
createCanvas = do
  glconfig <- GtkGL.glConfigNew [GtkGL.GLModeRGBA,
                                 GtkGL.GLModeDepth,
                                 GtkGL.GLModeDouble]

  canvas <- GtkGL.glDrawingAreaNew glconfig
  widgetSetSizeRequest canvas 800 600

  -- Set the repaint handler
  onExpose canvas $ \_ -> 
    GtkGL.withGLDrawingArea canvas $ \glwindow -> do
      clear [DepthBuffer, ColorBuffer]
      display
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
    shadeModel $= Smooth
    clearColor $= Color4 0.0 0.0 0.0 1.0
    clearDepth $= 1.0
    depthFunc $= Just Lequal
    color (Color3 1.0 1.0 1.0 :: Color3 GLfloat)
    matrixMode $= Projection
    loadIdentity
    drawBuffer $= BackBuffers
    hint PerspectiveCorrection $= Nicest
    -- lighting
    let l = Light 0
    light l $= Enabled
    lighting $= Enabled
    position l $= vertex4f 0.0 0.0 0.0 1.0
    colorMaterial $= Just (Front, Diffuse)
    lineSmooth $= Enabled
    hint LineSmooth $= Nicest
    hint PolygonSmooth $= Nicest
    
    

canvasOnExpose :: GtkGL.GLDrawingArea -> IO Bool
canvasOnExpose canvas =
    GtkGL.withGLDrawingArea canvas $ \glwindow -> do
      display
      GtkGL.glDrawableSwapBuffers glwindow
      return True

display :: IO ()
display = do
  clear [DepthBuffer, ColorBuffer]
  loadIdentity

  lookAt (vertex3d 0.0 0.0 1000.0) (vertex3d 0.0 0.0 0.0) (vector3d 0.0 1.0 0.0)
  rotate 0 (vector3f 1.0 0.0 0.0)
  rotate 0 (vector3f 0.0 1.0 0.0)

  drawSun
  
        -- loadIdentity 
        -- color (Color3 1 1 1 :: Color3 GLfloat)
        -- -- Instead of glBegin ... glEnd there is renderPrimitive.  
        -- renderPrimitive Polygon $ do
        -- vertex (Vertex3 0.25 0.25 0.0 :: Vertex3 GLfloat)
        -- vertex (Vertex3 0.75 0.25 0.0 :: Vertex3 GLfloat)
        -- vertex (Vertex3 0.75 0.75 0.0 :: Vertex3 GLfloat)
        -- vertex (Vertex3 0.25 0.75 0.0 :: Vertex3 GLfloat)

drawSun :: IO ()
drawSun = do
  color (Color3 1.0 1.0 0.0 :: Color3 GLfloat)
  materialEmission Front $= (Color4 1.0 1.0 0.0 1.0 :: Color4 GLfloat)
  renderQuadric (QuadricStyle Nothing NoTextureCoordinates Outside FillStyle) (Sphere 5.0 100 100)
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


