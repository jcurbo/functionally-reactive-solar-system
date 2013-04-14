module Main (main) where

import Graphics.UI.Gtk hiding (get)
import qualified Graphics.UI.Gtk.OpenGL as GtkGL

-- This lets us use := for Gtk attributes
import Graphics.UI.Gtk (AttrOp((:=)))

import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.Gtk

import ReactiveSolar.GUI
import ReactiveSolar.Data

main :: IO ()
main = do
       initGUI
       GtkGL.initGL

       sysState <- initState

       -- GUI construction
        
       window <- windowNew
       set window [containerBorderWidth := 8,
                   windowTitle := "Functionally Reactive Solar System" ]

       vBoxTop <- vBoxNew False 0
       set window [containerChild := vBoxTop]

       -- First row, buttons

       -- hBoxTop <- hBoxNew False 0
       -- boxPackStart vBoxTop hBoxTop PackNatural 0

       -- buttonStart <- buttonNewWithLabel "Start"
       -- boxPackStart hBoxTop buttonStart PackNatural 2

       -- buttonStop <- buttonNewWithLabel "Stop"
       -- boxPackStart hBoxTop buttonStop PackNatural 2

       -- buttonList <- buttonNewWithLabel "List Objs"
       -- boxPackStart hBoxTop buttonList PackNatural 2

       -- buttonAdd <- buttonNewWithLabel "Add Obj"
       -- boxPackStart hBoxTop buttonAdd PackNatural 2

       -- buttonRem <- buttonNewWithLabel "Rem Obj"
       -- boxPackStart hBoxTop buttonRem PackNatural 2

       -- Second row, tweak controls

       hBoxTweak <- hBoxNew False 0
       boxPackStart vBoxTop hBoxTweak PackNatural 0

       buttonQuit <- buttonNewWithLabel "Quit"
       boxPackEnd hBoxTweak buttonQuit PackNatural 2

       labelCamX <- labelNewWithMnemonic "Tilt"
       tiltVal <- getTilt sysState
       adjCamX <- adjustmentNew tiltVal (-90.0) 0 1 5.0 0
       spinCamX <- spinButtonNew adjCamX 1 0
       boxPackStart hBoxTweak labelCamX PackNatural 2
       boxPackStart hBoxTweak spinCamX PackNatural 2

       labelCamY <- labelNewWithMnemonic "Rotate"
       rotVal <- getRot sysState
       adjCamY <- adjustmentNew rotVal (-180.0) 180.0 1 5.0 0
       spinCamY <- spinButtonNew adjCamY 1 0
       spinButtonSetWrap spinCamY True
       boxPackStart hBoxTweak labelCamY PackNatural 2
       boxPackStart hBoxTweak spinCamY PackNatural 2

       labelCamZ <- labelNewWithMnemonic "Zoom"
       zoomVal <- getZoom sysState
       adjCamZ <- adjustmentNew zoomVal (-200.0) (-0.05) 0.5 5.0 0
       spinCamZ <- spinButtonNew adjCamZ 0.5 1
       boxPackStart hBoxTweak labelCamZ PackNatural 2
       boxPackStart hBoxTweak spinCamZ PackNatural 2

       labelScale <- labelNewWithMnemonic "Time Compression"
       scaleVal <- getScale sysState
       adjScale <- adjustmentNew (fromIntegral scaleVal) 1 1000000 100 1000 0
       spinScale <- spinButtonNew adjScale 1 0
       boxPackStart hBoxTweak labelScale PackNatural 2
       boxPackStart hBoxTweak spinScale PackNatural 2

       buttonScaleDay <- buttonNewWithLabel "1s = 1d"
       boxPackStart hBoxTweak buttonScaleDay PackNatural 2

       buttonScaleYear <- buttonNewWithLabel "1s = 1y"
       boxPackStart hBoxTweak buttonScaleYear PackNatural 2

       buttonReset <- buttonNewWithLabel "Reset"
       boxPackStart hBoxTweak buttonReset PackNatural 2
       
       -- OpenGL widget

       canvas <- createCanvas sysState
       boxPackStart vBoxTop canvas PackGrow 0

       -- bottom of window

       statusBar <- statusbarNew
       boxPackStart vBoxTop statusBar PackNatural 0

       delay <- getDelay sysState
   
       -- event network (events and handlers)
       network <- compile $ do
         eCanvasRealize <- event0 canvas realize
         -- eStart <- event0 buttonStart buttonActivated
         -- eStop  <- event0 buttonStop buttonActivated
         -- eList  <- event0 buttonList buttonActivated
         -- eAdd   <- event0 buttonAdd buttonActivated
         -- eRem   <- event0 buttonRem buttonActivated
         eQuit  <- event0 buttonQuit buttonActivated
         eReset <- event0 buttonReset buttonActivated

         eScaleDay <- event0 buttonScaleDay buttonActivated
         eScaleYear <- event0 buttonScaleYear buttonActivated

         ticks <- intervals delay

         reactimate $ updateState sysState <$ ticks

         reactimate $ canvasOnRealize canvas <$ eCanvasRealize
         -- reactimate $ buttonStartAct <$ eStart
         -- reactimate $ buttonStopAct <$ eStop
         -- reactimate $ buttonListAct <$ eList
         -- reactimate $ buttonAddAct <$ eAdd
         -- reactimate $ buttonRemAct <$ eRem
         reactimate $ buttonQuitAct window <$ eQuit
         reactimate $ buttonResetAct sysState spinCamX spinCamY spinCamZ <$ eReset

         reactimate $ buttonScaleDayAct sysState spinScale <$ eScaleDay
         reactimate $ buttonScaleYearAct sysState spinScale <$ eScaleYear

       actuate network

       -- GTK events are handled here because RB.Gtk doesn't have a way to tie them
       -- in yet (vs GTK signals) (that I can figure out)
       -- there's a commented out eventM in the RB.Gtk source that would do it but
       -- it doesn't seem to work yet

       onValueSpinned spinCamX $
         updateCam sysState spinCamX spinCamY spinCamZ
       onValueSpinned spinCamY $
         updateCam sysState spinCamX spinCamY spinCamZ
       onValueSpinned spinCamZ $
         updateCam sysState spinCamX spinCamY spinCamZ
       onValueSpinned spinScale $
         updateScaleFromSpinner sysState spinScale

       onDestroy window mainQuit
       
       widgetShowAll window
       mainGUI
