{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module FileChooser where

import qualified GI.Gtk as Gtk
import qualified GI.GdkPixbuf as PB
import GI.GObject
import Data.Text
import Data.Maybe
import Button
import Window

selectFile builder = do
    setButtonActive False builder
    setWindowActive False builder
    native <- new Gtk.FileChooserNative []
    res <- Gtk.nativeDialogRun native
    filename <-  Gtk.fileChooserGetFilename native
    setButtonActive True builder
    setWindowActive True builder
    case filename of
        (Just file) -> do 
            displayOnCanvas file builder
            return ()
        _ -> return ()
    return ()

displayOnCanvas file builder = do
    canvas <- getGTKWidget Gtk.Image "canvas" builder
    check <- PB.pixbufGetFileInfo file
    case check of 
        (Just img, _, _) -> do
            pb <- PB.pixbufNewFromFileAtSize file 500 500
            -- HEH
            pbCopy <- PB.pixbufCopy pb
            _ <- Gtk.imageSetFromPixbuf canvas pbCopy
            handleExporting builder
            return ()
        _-> do
            print "That file wasn't an image"
            return ()
    return ()


handleExporting builder = do
    exportButton <- getGTKWidget  Gtk.Button "export" builder
    Gtk.on exportButton #clicked $ exportFile builder
    return ()

exportFile builder = do
    canvas <- getGTKWidget Gtk.Image "canvas" builder
    maybePixbuf <- Gtk.imageGetPixbuf canvas

    case maybePixbuf of 
        (Just pb) -> do 
            setButtonActive False builder
            setWindowActive False builder
            native <- new Gtk.FileChooserNative []
            Gtk.fileChooserSetAction native Gtk.FileChooserActionSave
            res <- Gtk.nativeDialogRun native
            filename <-  Gtk.fileChooserGetFilename native
            setButtonActive True builder
            setWindowActive True builder
            case filename of
                (Just file) -> PB.pixbufSavev pb (file ++ ".png") "png" [] []
                _ -> return ()
            return ()
        _ -> return ()
    return()
