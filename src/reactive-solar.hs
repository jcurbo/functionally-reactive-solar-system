module Main (main) where

import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.OpenGL as GtkGL
import Graphics.Rendering.OpenGL as GL
-- This lets us use := for Gtk attributes
import Graphics.UI.Gtk (AttrOp((:=)))

import Reactive.Banana
import Reactive.Banana.Frameworks

-- import Control.Applicative

main :: IO ()
main = do
        Gtk.initGUI

        GtkGL.initGL

        glconfig <- GtkGL.glConfigNew [GtkGL.GLModeRGBA,
                                      GtkGL.GLModeDepth,
                                      GtkGL.GLModeDouble]

        canvas <- GtkGL.glDrawingAreaNew glconfig
        Gtk.widgetSetSizeRequest canvas 800 600

        Gtk.onRealize canvas $ GtkGL.withGLDrawingArea canvas $ \_ -> do
            clearColor $= Color4 0.0 0.0 0.0 0.0
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


        window <- Gtk.windowNew
        Gtk.onDestroy window Gtk.mainQuit
        Gtk.set window [ Gtk.containerBorderWidth := 8,
                         Gtk.windowTitle := "Functionally Reactive Solar System" ]

        -- more GUI stuff goes here
        
        vbox <- Gtk.vBoxNew False 4
        Gtk.set window [ Gtk.containerChild := vbox ]
 
        label <- Gtk.labelNew (Just "Gtk2Hs using OpenGL via HOpenGL!")
        closeButton <- Gtk.buttonNewWithLabel "Close"
        Gtk.onClicked closeButton Gtk.mainQuit
        Gtk.set vbox [ Gtk.containerChild := canvas,
                       Gtk.containerChild := label,
                       Gtk.containerChild := closeButton ]


        network <- compile $ do
          edestroy <- fromAddHandler $ registerDestroyEvent window
          eclickclose <- fromAddHandler $ registerClickEvent closeButton

          reactimate $ Gtk.widgetDestroy window <$ eclickclose
          reactimate $ Gtk.mainQuit <$ edestroy

        actuate network
        Gtk.widgetShowAll window
        Gtk.mainGUI

-- OpenGL stuff goes here

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
    
animationWaitTime = 3

-- Gtk stuff

registerDestroyEvent :: Gtk.WidgetClass w => w -> (() -> IO ()) -> IO (IO ())
registerDestroyEvent window handler = do
    connectionID <- Gtk.onDestroy window (handler $ ())
    let d = Gtk.signalDisconnect connectionID
    return d
    
registerClickEvent :: Gtk.ButtonClass b => b  -> (() -> IO ()) -> IO (IO ())
registerClickEvent button handler = do
    connectionID <- Gtk.onClicked button (handler $ ())
    let d = Gtk.signalDisconnect connectionID
    return d
