{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. NetworkDescription t"
import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.Combinators
import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk (AttrOp((:=)))


main :: IO ()
main = do
    Gtk.initGUI
    window <- Gtk.windowNew
    label <- Gtk.labelNew $ Just "label1"
    entry <- Gtk.entryNew
    applyButton <- Gtk.buttonNewWithLabel "Apply"
    closeButton <- Gtk.buttonNewWithLabel "Close"
    vBox <- Gtk.vBoxNew False 2
    hBox <- Gtk.hBoxNew False 2
    Gtk.set window [Gtk.containerChild := vBox,
                    Gtk.windowTitle := "hello frp world"]
    Gtk.set vBox [Gtk.containerChild := label,
                  Gtk.containerChild := hBox]
    Gtk.set hBox [Gtk.containerChild := applyButton,
                  Gtk.containerChild := closeButton]

    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
           edestroy      <- fromAddHandler $ registerDestroyEvent window
           eclickapply   <- fromAddHandler $ registerClickEvent applyButton
           eclickclose   <- fromAddHandler $ registerClickEvent closeButton

           reactimate $ fmap (\x -> Gtk.widgetDestroy window ) eclickclose
           reactimate $ fmap (\x -> displayName label entry) eclickapply
           reactimate $ fmap (\x -> Gtk.mainQuit ) edestroy

    network <- compile networkDescription
    actuate network
    Gtk.widgetShowAll window
    Gtk.mainGUI


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

displayName label entry = do
    name <- Gtk.get entry Gtk.entryText
    Gtk.set label [ Gtk.labelText := "Hello " ++ name ]
