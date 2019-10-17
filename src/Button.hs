{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module Button where 
import qualified GI.Gtk as Gtk
import qualified Graphics.Image as I
import Window
import Convolutions

setButtonActive name status builder = do 
    button <- getGTKWidget Gtk.Button name builder
    Gtk.widgetSetSensitive button status

initializeButtons builder = do
    rotateButton <- getGTKWidget  Gtk.Button "rotate" builder
    Gtk.on rotateButton #clicked $ rotate builder
    
    monochromeButton <- getGTKWidget  Gtk.Button "monochrome" builder
    Gtk.on monochromeButton #clicked $ mytransformationY builder
    
    flipvButton <- getGTKWidget  Gtk.Button "flipv" builder
    Gtk.on flipvButton #clicked $ myVtransformationRGB builder

    fliphButton <- getGTKWidget  Gtk.Button "fliph" builder
    Gtk.on fliphButton #clicked $ myHtransformationRGB builder

    sharpenButton <- getGTKWidget  Gtk.Button "sharpen" builder
    Gtk.on sharpenButton #clicked $ convolve sharpen builder

    blurButton <- getGTKWidget  Gtk.Button "blur" builder
    Gtk.on blurButton #clicked $ convolve blur builder
   
    embossButton <- getGTKWidget  Gtk.Button "emboss" builder
    Gtk.on embossButton #clicked $ convolve emboss builder
    
    outlineButton <- getGTKWidget  Gtk.Button "outline" builder
    Gtk.on outlineButton #clicked $ convolve outline builder
    return ()

setConvolutionsActive status builder = do
    button <- getGTKWidget  Gtk.Button "rotate" builder
    Gtk.widgetSetSensitive button status
    button <- getGTKWidget  Gtk.Button "monochrome" builder
    Gtk.widgetSetSensitive button status
    button <- getGTKWidget  Gtk.Button "flipv" builder
    Gtk.widgetSetSensitive button status
    button <- getGTKWidget  Gtk.Button "fliph" builder
    Gtk.widgetSetSensitive button status
    button <- getGTKWidget  Gtk.Button "sharpen" builder
    Gtk.widgetSetSensitive button status
    button <- getGTKWidget  Gtk.Button "blur" builder
    Gtk.widgetSetSensitive button status
    button <- getGTKWidget  Gtk.Button "emboss" builder
    Gtk.widgetSetSensitive button status
    button <- getGTKWidget  Gtk.Button "outline" builder
    Gtk.widgetSetSensitive button status
