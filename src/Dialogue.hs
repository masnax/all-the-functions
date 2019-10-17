{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module Dialogue where

import qualified GI.Gtk as Gtk
import qualified GI.GdkPixbuf as PB
import Data.GI.Base
import GI.GObject
import Data.Text
import Data.Maybe
import Data.ByteString
import Button
import Window

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
            -- HEH
            pbCopy <- PB.pixbufCopy pb
            _ <- Gtk.imageSetFromPixbuf canvas pbCopy
            -- activate buttons
            activateButtons builder

            return ()
        _-> do
            print "That file wasn't an image"
            return ()
    return ()


activateButtons builder = do
    exportButton <- getGTKWidget  Gtk.Button "export" builder
    on exportButton #clicked $ exportFile builder

    rotateButton <- getGTKWidget  Gtk.Button "rotate" builder
    on rotateButton #clicked $ convolute rotate builder

    monochromeButton <- getGTKWidget  Gtk.Button "monochrome" builder
    on monochromeButton #clicked $ convolute monochrome builder
    return ()

monochrome pb = do
    bytestring <- PB.pixbufGetPixels pb
    let arr = (Data.ByteString.unpack bytestring)
    print arr
    return (Just pb)

getPixbufMetadata pb = do  
    bytestring <- PB.pixbufGetPixels pb
    c <- (PB.pixbufGetColorspace pb)
    a <- (PB.pixbufGetHasAlpha pb)
    s <- (PB.pixbufGetBitsPerSample pb)
    w <- (PB.pixbufGetWidth pb)
    h <- (PB.pixbufGetHeight pb)
    r <- (PB.pixbufGetRowstride pb)
    return ((Data.ByteString.unpack bytestring), c, w, h)


rotate pb = do
  PB.pixbufRotateSimple pb PB.PixbufRotationCounterclockwise

convolute convolution builder = do
    -- get img using canvas
    canvas <- getGTKWidget Gtk.Image "canvas" builder
    maybePixbuf <- Gtk.imageGetPixbuf canvas

    case maybePixbuf of
    -- if type of pixbuf is equal to "just pb"
      (Just pb) -> do
        pbConvoluted <- (convolution pb)
        -- once its rotated, show on canvas
        _ <- Gtk.imageSetFromPixbuf canvas pbConvoluted
        return ()
      _ -> return ()
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
