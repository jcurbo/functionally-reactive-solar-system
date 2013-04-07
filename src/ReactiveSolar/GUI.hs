module ReactiveSolar.GUI where

import Graphics.UI.Gtk
import Graphics.UI.Gtk (AttrOp((:=)))
import qualified Graphics.UI.Gtk.OpenGL as GtkGL

import Graphics.Rendering.OpenGL as GL


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
      GL.clear [GL.DepthBuffer, GL.ColorBuffer]
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
    clearColor $= Color4 0.0 0.0 0.0 0.0
    matrixMode $= Projection
    loadIdentity
    ortho 0.0 1.0 0.0 1.0 (-1.0) 1.0
    depthFunc $= Just Less
    drawBuffer $= BackBuffers

canvasOnExpose :: GtkGL.GLDrawingArea -> IO Bool
canvasOnExpose canvas =
    GtkGL.withGLDrawingArea canvas $ \glwindow -> do
      GL.clear [GL.DepthBuffer, GL.ColorBuffer]
      display
      GtkGL.glDrawableSwapBuffers glwindow
      return True

display :: IO ()
display = do
        loadIdentity 
        color (Color3 1 1 1 :: Color3 GLfloat)
        -- Instead of glBegin ... glEnd there is renderPrimitive.  
        renderPrimitive Polygon $ do
        vertex (Vertex3 0.25 0.25 0.0 :: Vertex3 GLfloat)
        vertex (Vertex3 0.75 0.25 0.0 :: Vertex3 GLfloat)
        vertex (Vertex3 0.75 0.75 0.0 :: Vertex3 GLfloat)
        vertex (Vertex3 0.25 0.75 0.0 :: Vertex3 GLfloat)

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
  

