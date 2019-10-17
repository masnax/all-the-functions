{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module Dialogue where

import qualified GI.Gtk as Gtk
import qualified GI.GdkPixbuf as PB
import qualified Graphics.Image as I
import Data.GI.Base
import GI.GObject
import Data.Text
import Data.Maybe
import Data.ByteString
import Button
import Window
import Convolutions


selectFile builder = do
    setWindowActive False builder
    native <- new Gtk.FileChooserNative []
    killDialog native builder
    _ <- Gtk.nativeDialogRun native
    filename <-  Gtk.fileChooserGetFilename native
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
            PB.pixbufSavev pb "./src/images/hidimg.png" "png" [] []
            pbCopy <- PB.pixbufCopy pb
            _ <- Gtk.imageSetFromPixbuf canvas pbCopy
            -- activate buttons
            exportButton <- getGTKWidget  Gtk.Button "export" builder
            on exportButton #clicked $ exportFile builder
            activateButtons builder

            return ()
        _-> do
            print "That file wasn't an image"
            return ()
    return ()

resetImage file builder = do
    canvas <- getGTKWidget Gtk.Image "canvas" builder
    check <- PB.pixbufGetFileInfo file
    case check of
        (Just img, _, _) -> do
            pb <- PB.pixbufNewFromFileAtSize file 500 500
            PB.pixbufSavev pb "./src/images/hidimg.png" "png" [] []
            pbCopy <- PB.pixbufCopy pb
            _ <- Gtk.imageSetFromPixbuf canvas pbCopy
            return ()
        _-> do
            print "That file wasn't an image"
            return ()
    return ()

exportFile builder = do
    canvas <- getGTKWidget Gtk.Image "canvas" builder
    maybePixbuf <- Gtk.imageGetPixbuf canvas

    case maybePixbuf of
        (Just pb) -> do
            setWindowActive False builder
            native <- new Gtk.FileChooserNative []
            killDialog native builder
            Gtk.fileChooserSetAction native Gtk.FileChooserActionSave
            res <- Gtk.nativeDialogRun native
            filename <-  Gtk.fileChooserGetFilename native
            setWindowActive True builder
            case filename of
                (Just file) -> PB.pixbufSavev pb (file ++ ".png") "png" [] []
                _ -> return ()
            return ()
        _ -> return ()
    return()
