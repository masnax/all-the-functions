{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}


module Convolutions where

import qualified GI.Gtk as Gtk
import qualified GI.GdkPixbuf as PB
import qualified Graphics.Image as I
import Data.GI.Base
import GI.GObject
import Data.Text
import Data.Maybe
import Data.ByteString
import Window

-- Matrices for image manipulation
sharpen,blur,emboss,outline :: (Fractional a) => [[a]]
sharpen = [[0,-1,0],[-1,5,-1],[0,-1,0]]
blur = [[0.0625,0.125,0.0625],[0.125,0.25,0.125],[0.0625,0.125,0.0625]]
emboss = [[-1,-0.5,0],[-0.5,0.5,0.5],[0,0.5,1]]
outline = [[-0.5,-0.5,-0.5],[-0.5,4,-0.5],[-0.5,-0.5,-0.5]]

-- Updates the image in the images folder to grayscale and calls updateCanvas
transformMonochrome :: Gtk.Builder -> IO ()
transformMonochrome builder = do
      hidimg <- I.readImageRGB I.VU "./src/images/hidimg.png"
      let img = I.toImageY hidimg
      _ <- I.writeImage "./src/images/hidimg.png" img
      updateCanvas builder

-- Vertically flips the image in the images folder and calls updateCanvas
transformFlipVertical :: Gtk.Builder -> IO ()
transformFlipVertical builder = do
    hidimg <- I.readImageRGB I.VU "./src/images/hidimg.png"
    let img = I.flipV hidimg
    _ <- I.writeImage "./src/images/hidimg.png" img
    updateCanvas builder

-- Horizontally flips the image in the images folder and calls updateCanvas
transformFlipHorizontal :: Gtk.Builder -> IO ()
transformFlipHorizontal builder = do
    hidimg <- I.readImageRGB I.VU "./src/images/hidimg.png"
    let img = I.flipH hidimg
    _ <- I.writeImage "./src/images/hidimg.png" img
    updateCanvas builder

-- Given a matrix kernel, applies the corresponding convolution to the pixel map of the image
convolve :: [[I.Pixel I.X Double ]] -> Gtk.Builder -> IO ()
convolve kernel builder = do
      hidimg <- I.readImageRGB I.VU "./src/images/hidimg.png"
      let img = I.convolve I.Reflect (I.fromLists kernel) hidimg
      _ <- I.writeImage "./src/images/hidimg.png" img
      updateCanvas builder

-- Rotates the pixbuf counter-clockwise by 90 degrees
rotate :: Gtk.Builder -> IO ()
rotate builder = do
    canvas <- getGTKWidget Gtk.Image "canvas" builder
    maybePixbuf <- Gtk.imageGetPixbuf canvas
    case maybePixbuf of
      (Just pb) -> do
        pbRotate <- PB.pixbufRotateSimple pb PB.PixbufRotationCounterclockwise
        Gtk.imageSetFromPixbuf canvas pbRotate
        maybePixbuf <- Gtk.imageGetPixbuf canvas
        case maybePixbuf of
            (Just pbr) -> PB.pixbufSavev pbr "./src/images/hidimg.png" "png" [] []
            _ -> return ()
      _ -> return ()

-- Updates the current canvas' pixbuf with the image in the images folder
updateCanvas :: Gtk.Builder -> IO ()
updateCanvas builder = do
  canvas <- getGTKWidget Gtk.Image "canvas" builder
  check <- PB.pixbufGetFileInfo  "./src/images/hidimg.png"
  case check of
    (Just img, _, _) -> do
        pb <- PB.pixbufNewFromFileAtSize  "./src/images/hidimg.png" 500 500
        pbCopy <- PB.pixbufCopy pb
        Gtk.imageSetFromPixbuf canvas pbCopy
    _-> do
        print "That file wasn't an image"
        return ()
