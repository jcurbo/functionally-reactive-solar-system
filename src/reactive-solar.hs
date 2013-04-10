module Main (main) where

import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.OpenGL as GtkGL
import Graphics.Rendering.OpenGL

-- This lets us use := for Gtk attributes
import Graphics.UI.Gtk (AttrOp((:=)))

import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.Gtk

import ReactiveSolar.GUI
import ReactiveSolar.Orbit
import ReactiveSolar.Data

import Data.IORef



main :: IO ()
main = do
        initGUI
        GtkGL.initGL

        let cam = CameraState 30.0 0 0.5

        camState <- newIORef cam

        window <- windowNew
        set window [containerBorderWidth := 8,
                         windowTitle := "Functionally Reactive Solar System" ]

        vBoxTop <- vBoxNew False 0
        set window [containerChild := vBoxTop]

        -- First row, buttons

        hBoxTop <- hBoxNew False 0
        boxPackStart vBoxTop hBoxTop PackNatural 0

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

        -- Second row, tweak controls

        hBoxTweak <- hBoxNew False 0
        boxPackStart vBoxTop hBoxTweak PackNatural 0

        labelCamX <- labelNewWithMnemonic "Tilt"
        adjCamX <- adjustmentNew (tilt cam) (-90.0) 90.0 1 5.0 0
        spinCamX <- spinButtonNew adjCamX 1 0
        boxPackStart hBoxTweak labelCamX PackNatural 2
        boxPackStart hBoxTweak spinCamX PackNatural 2

        labelCamY <- labelNewWithMnemonic "Rotate"
        adjCamY <- adjustmentNew (rot cam) (-180.0) 180.0 1 5.0 0
        spinCamY <- spinButtonNew adjCamY 1 0
        spinButtonSetWrap spinCamY True
        boxPackStart hBoxTweak labelCamY PackNatural 2
        boxPackStart hBoxTweak spinCamY PackNatural 2

        labelCamZ <- labelNewWithMnemonic "Zoom"
        adjCamZ <- adjustmentNew (zoom cam) (-10.0) (-0.05) 0.05 1.0 0
        spinCamZ <- spinButtonNew adjCamZ 0.05 2
        boxPackStart hBoxTweak labelCamZ PackNatural 2
        boxPackStart hBoxTweak spinCamZ PackNatural 2

        buttonReset <- buttonNewWithLabel "Reset"
        boxPackStart hBoxTweak buttonReset PackNatural 2

        canvas <- createCanvas camState
        boxPackStart vBoxTop canvas PackGrow 0

        -- add label here later for time?

        statusBar <- statusbarNew
        boxPackStart vBoxTop statusBar PackNatural 0
   
        -- event network (events and handlers)
        network <- compile $ do
          eCanvasRealize <- event0 canvas realize
          eStart <- event0 buttonStart buttonActivated
          eStop  <- event0 buttonStop buttonActivated
          eList  <- event0 buttonList buttonActivated
          eAdd   <- event0 buttonAdd buttonActivated
          eRem   <- event0 buttonRem buttonActivated
          eQuit  <- event0 buttonQuit buttonActivated
          eReset <- event0 buttonReset buttonActivated

        
       
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
          reactimate $ buttonResetAct camState spinCamX spinCamY spinCamZ <$ eReset

        actuate network

        -- handled here because RB.Gtk doesn't have a way to tie in
        -- GTK events (vs signals) yet (that I can figure out)

        onValueSpinned spinCamX $
          updateCam camState spinCamX spinCamY spinCamZ
        onValueSpinned spinCamY $
          updateCam camState spinCamX spinCamY spinCamZ
        onValueSpinned spinCamZ $
          updateCam camState spinCamX spinCamY spinCamZ

        onDestroy window mainQuit
       
        widgetShowAll window
        mainGUI





