module Main (main) where

import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.OpenGL as GtkGL
-- import Graphics.Rendering.OpenGL as GL

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

        vboxTop <- vBoxNew False 0
        set window [containerChild := vboxTop]

        -- menu <- createMenu
        -- (Just menuBar) <- uiManagerGetWidget menu "/ui/menubar"
        -- boxPackStart vboxTop menuBar PackNatural 0

        hBoxTop <- hBoxNew False 0
        boxPackStart vboxTop hBoxTop PackNatural 0

        buttonStart <- buttonNewWithLabel "Start"
        boxPackStart hBoxTop buttonStart PackNatural 0

        buttonStop <- buttonNewWithLabel "Stop"
        boxPackStart hBoxTop buttonStop PackNatural 0

        buttonList <- buttonNewWithLabel "List Objs"
        boxPackStart hBoxTop buttonList PackNatural 0

        buttonAdd <- buttonNewWithLabel "Add Obj"
        boxPackStart hBoxTop buttonAdd PackNatural 0

        buttonRem <- buttonNewWithLabel "Rem Obj"
        boxPackStart hBoxTop buttonRem PackNatural 0

        buttonQuit <- buttonNewWithLabel "Quit"
        boxPackEnd hBoxTop buttonQuit PackNatural 0

        canvas <- createCanvas
        boxPackStart vboxTop canvas PackGrow 0

        -- add label here later for time?

        statusBar <- statusbarNew
        boxPackStart vboxTop statusBar PackNatural 0
   
        -- event network (events and handlers)
        network <- compile $ do
          eCanvasRealize <- event0 canvas realize
          eStart <- event0 buttonStart buttonActivated
          eStop  <- event0 buttonStop buttonActivated
          eList  <- event0 buttonList buttonActivated
          eAdd   <- event0 buttonAdd buttonActivated
          eRem   <- event0 buttonRem buttonActivated
          eQuit  <- event0 buttonQuit buttonActivated
          
        --   eChange <- event0 changeButton buttonActivated
        --   eClose <- event0 closeButton buttonActivated
        --   eToggle <- event0 toggleButton toggled

        --   reactimate $ widgetDestroy window <$ eClose
        --   reactimate $ set label [labelText := "WOW"] <$ eChange
        --   reactimate $ toggleButtonSetMode toggleButton True <$ eToggle
          reactimate $ canvasOnRealize canvas <$ eCanvasRealize
          reactimate $ buttonStartAct <$ eStart
          reactimate $ buttonStopAct <$ eStop
          reactimate $ buttonListAct <$ eList
          reactimate $ buttonAddAct <$ eAdd
          reactimate $ buttonRemAct <$ eRem
          reactimate $ buttonQuitAct window <$ eQuit

        actuate network

        -- handled here because RB.Gtk doesn't have a way to tie in
        -- GTK events (vs signals) yet (that I can figure out)

        onDestroy window mainQuit
       
        widgetShowAll window
        mainGUI





