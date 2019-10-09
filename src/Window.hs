{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module Window where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import Data.Maybe
import Data.Text


getGTKWidget gtkType id builder = do  
    maybeWidget <- Gtk.builderGetObject builder (pack id)
    Gtk.unsafeCastTo gtkType (fromJust maybeWidget)

setWindowActive status builder = do 
    window <- getGTKWidget Gtk.Window "window" builder
    Gtk.widgetSetSensitive window status

killDialog dialog builder = do 
    window <- getGTKWidget Gtk.Window "window" builder
    on window #destroy $ Gtk.nativeDialogDestroy dialog
