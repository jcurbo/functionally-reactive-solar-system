module Main (main) where

import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.OpenGL as GtkGL
import Graphics.Rendering.OpenGL as GL
-- This lets us use := for Gtk attributes
import Graphics.UI.Gtk (AttrOp((:=)))

import Reactive.Banana
import Reactive.Banana.Frameworks

import Reactive.Banana.Gtk

import ReactiveSolar.GUI
import ReactiveSolar.Orbit
import ReactiveSolar.Data

main :: IO ()
main = do
        initGUI
        GtkGL.initGL

        window <- windowNew
        set window [containerBorderWidth := 8,
                         windowTitle := "Functionally Reactive Solar System" ]

        vboxTop <- vBoxNew False 4
        set window [containerChild := vboxTop]

        menu <- createMenu
        (Just menuBar) <- uiManagerGetWidget menu "/ui/menubar"
        boxPackStart vboxTop menuBar PackNatural 0

        canvas <- createCanvas
        boxPackStart vboxTop canvas PackGrow 0

        statusBar <- statusbarNew
        boxPackStart vboxTop statusBar PackNatural 0
   
        -- event network (events and handlers)
        network <- compile $ do
          eCanvasRealize <- event0 canvas realize

        --   eChange <- event0 changeButton buttonActivated
        --   eClose <- event0 closeButton buttonActivated
        --   eToggle <- event0 toggleButton toggled

        --   reactimate $ widgetDestroy window <$ eClose
        --   reactimate $ set label [labelText := "WOW"] <$ eChange
        --   reactimate $ toggleButtonSetMode toggleButton True <$ eToggle
          reactimate $ canvasOnRealize canvas <$ eCanvasRealize

        actuate network

        -- handled here because RB.Gtk doesn't have a way to tie in
        -- GTK events (vs signals) yet (that I can figure out)

        onDestroy window mainQuit
       
        widgetShowAll window
        mainGUI





