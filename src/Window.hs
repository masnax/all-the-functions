{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module Window where

import qualified GI.Gtk as Gtk
import GI.GObject
import Data.GI.Base
import Data.Maybe
import Data.Text

{-
Creates the appropriate widget from the glade file builder using the given id and a GTK widget type
-}
getGTKWidget :: (GI.GObject.GObject w) => (Data.GI.Base.ManagedPtr w -> w) ->  String ->  Gtk.Builder -> IO w
getGTKWidget gtkType id builder = do
    maybeWidget <- Gtk.builderGetObject builder (pack id)
    Gtk.unsafeCastTo gtkType (fromJust maybeWidget)

{-
Given status, sets window interactivity to status.
-}
setWindowActive :: Bool -> Gtk.Builder -> IO ()
setWindowActive status builder = do
    window <- getGTKWidget Gtk.Window "window" builder
    Gtk.widgetSetSensitive window status

{-
Kills the given file chooser when the main window is closed.
-}
killDialog :: Gtk.FileChooserNative -> Gtk.Builder -> IO ()
killDialog dialog builder = do
    window <- getGTKWidget Gtk.Window "window" builder
    on window #destroy $ Gtk.nativeDialogDestroy dialog
    return ()
