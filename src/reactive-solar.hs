{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. NetworkDescription t"
module Main (main) where

import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.OpenGL as GtkGL
import Graphics.Rendering.OpenGL as GL
-- This lets us use := for Gtk attributes
import Graphics.UI.Gtk (AttrOp((:=)))

import Reactive.Banana
import Reactive.Banana.Frameworks

import Reactive.Banana.Gtk

-- import Control.Applicative

main :: IO ()
main = do
        initGUI

        GtkGL.initGL

        glconfig <- GtkGL.glConfigNew [GtkGL.GLModeRGBA,
                                      GtkGL.GLModeDepth,
                                      GtkGL.GLModeDouble]

        canvas <- GtkGL.glDrawingAreaNew glconfig
        widgetSetSizeRequest canvas 800 600

        onRealize canvas $ GtkGL.withGLDrawingArea canvas $ \_ -> do
            clearColor $= Color4 0.0 0.0 0.0 0.0
            matrixMode $= Projection
            loadIdentity
            ortho 0.0 1.0 0.0 1.0 (-1.0) 1.0
            depthFunc $= Just Less
            drawBuffer $= BackBuffers

        -- Set the repaint handler
        onExpose canvas $ \_ -> do
            GtkGL.withGLDrawingArea canvas $ \glwindow -> do
            GL.clear [GL.DepthBuffer, GL.ColorBuffer]
            display
            GtkGL.glDrawableSwapBuffers glwindow
            return True
        
        -- Setup the animation
        timeoutAddFull (do
            widgetQueueDraw canvas
            return True)
            priorityDefaultIdle animationWaitTime


        window <- windowNew
        set window [containerBorderWidth := 8,
                         windowTitle := "Functionally Reactive Solar System" ]

        -- more GUI stuff goes here
        
        vbox <- vBoxNew False 4
        set window [containerChild := vbox ]
 
        label <- labelNew (Just "Gtk2Hs using OpenGL via HOpenGL!")
        toggleButton <- toggleButtonNewWithLabel "Test Toggle"
        toggleButtonSetMode toggleButton False
        changeButton <- buttonNewWithLabel "Change"
        closeButton <- buttonNewWithLabel "Close"
        set vbox [ containerChild := canvas,
                       containerChild := label,
                       containerChild := changeButton,
                       containerChild := closeButton,
                       containerChild := toggleButton
                 ]

        -- event network (events and handlers)
        network <- compile $ do
          eChange <- event0 changeButton buttonActivated
          eClose <- event0 closeButton buttonActivated
          eToggle <- event0 toggleButton toggled

          reactimate $ widgetDestroy window <$ eClose
          reactimate $ set label [labelText := "WOW"] <$ eChange
          reactimate $ toggleButtonSetMode toggleButton True <$ eToggle


        actuate network

        onDestroy window mainQuit
        widgetShowAll window
        mainGUI

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


