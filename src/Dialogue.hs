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

{-
Opens the OS' native file chooser and sends the filepath of the selected file to displayOnCanvas
-}
selectFile :: Gtk.Builder -> IO ()
selectFile builder = do
    setWindowActive False builder
    native <- new Gtk.FileChooserNative []
    killDialog native builder
    _ <- Gtk.nativeDialogRun native
    filename <-  Gtk.fileChooserGetFilename native
    setWindowActive True builder
    case filename of
        (Just file) -> displayOnCanvas file builder
        _ -> return ()

{-
Updates the canvas' pixbuf value to the image at the given filepath, if and only if it is an image.
Image is converted to png and scaled to max 500x500px
A copy is stored in the images directory
Enables all buttons for image manipulation and exporting
Passes original filepath to resetImage
-}
displayOnCanvas :: [Char] -> Gtk.Builder -> IO ()
displayOnCanvas file builder = do
    canvas <- getGTKWidget Gtk.Image "canvas" builder
    check <- PB.pixbufGetFileInfo file
    case check of
        (Just img, _, _) -> do
            pb <- PB.pixbufNewFromFileAtSize file 500 500
            PB.pixbufSavev pb "./src/images/hidimg.png" "png" [] []
            pbCopy <- PB.pixbufCopy pb
            _ <- Gtk.imageSetFromPixbuf canvas pbCopy
            clearButton <- getGTKWidget  Gtk.Button "undo" builder
            on clearButton #clicked $ resetImage file builder
            setButtonActive "export" True builder
            setConvolutionsActive True builder
        _-> do print "That file wasn't an image"


{-
Sets the current pixbuf to the given image filepath.
Filepath is the original image chosen via import
-}
resetImage :: [Char] -> Gtk.Builder -> IO ()
resetImage file builder = do
    canvas <- getGTKWidget Gtk.Image "canvas" builder
    check <- PB.pixbufGetFileInfo file
    case check of
        (Just img, _, _) -> do
            pb <- PB.pixbufNewFromFileAtSize file 500 500
            PB.pixbufSavev pb "./src/images/hidimg.png" "png" [] []
            pbCopy <- PB.pixbufCopy pb
            Gtk.imageSetFromPixbuf canvas pbCopy
        _-> return ()

{-
Opens OS' native file chooser for saving files. Saves the file as a png at the given location with the given name
-}
exportFile :: Gtk.Builder -> IO ()
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
        _ -> return ()
