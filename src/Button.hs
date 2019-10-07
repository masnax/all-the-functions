module Button where 
import qualified GI.Gtk as Gtk
import Window


setButtonActive status builder = do 
    button <- getGTKWidget Gtk.Button "import" builder
    Gtk.widgetSetSensitive button status
    button <- getGTKWidget Gtk.Button "export" builder
    Gtk.widgetSetSensitive button status

