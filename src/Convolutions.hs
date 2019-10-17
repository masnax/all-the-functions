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
import Button
import Window

sharpen,blur,emboss,outline :: (Fractional a) => [[a]]
sharpen = [[0,-1,0],[-1,5,-1],[0,-1,0]]
blur = [[0.0625,0.125,0.0625],[0.125,0.25,0.125],[0.0625,0.125,0.0625]]
emboss = [[-3,-1,0],[-1,1,1],[0,1,2]]
outline = [[-1,-1,-1],[-1,8,-1],[-1,-1,-1]]

activateButtons builder = do
    rotateButton <- getGTKWidget  Gtk.Button "rotate" builder
    on rotateButton #clicked $ rotate builder
    
    monochromeButton <- getGTKWidget  Gtk.Button "monochrome" builder
    on monochromeButton #clicked $ mytransformation I.toImageY builder
    
--    flipvButton <- getGTKWidget  Gtk.Button "flipv" builder
--    on flipvButton #clicked $ mytransformation I.flipV builder
--
--    fliphButton <- getGTKWidget  Gtk.Button "fliph" builder
--    on fliphButton #clicked $ mytransformation I.flipH builder
    
    sharpenButton <- getGTKWidget  Gtk.Button "sharpen" builder
    on sharpenButton #clicked $ convolve sharpen builder

    blurButton <- getGTKWidget  Gtk.Button "blur" builder
    on blurButton #clicked $ convolve blur builder
   
    embossButton <- getGTKWidget  Gtk.Button "emboss" builder
    on embossButton #clicked $ convolve emboss builder
    
    outlineButton <- getGTKWidget  Gtk.Button "outline" builder
    on outlineButton #clicked $ convolve outline builder
    return ()

mytransformation func builder = do
      hidimg <- I.readImageRGB I.VU "./src/images/hidimg.png"
      let img = func hidimg
      _ <- I.writeImage "./src/images/hidimg.png" img
      _ <- updateCanvas builder
      return ()

convolve kernel builder = do
      hidimg <- I.readImageRGB I.VU "./src/images/hidimg.png"
      let img = I.convolve I.Reflect (I.fromLists kernel) hidimg
      _ <- I.writeImage "./src/images/hidimg.png" img
      _ <- updateCanvas builder
      return ()

rotate builder = do
    -- get img using canvas
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
    return ()

updateCanvas builder = do
  canvas <- getGTKWidget Gtk.Image "canvas" builder
  check <- PB.pixbufGetFileInfo  "./src/images/hidimg.png"
  case check of
    (Just img, _, _) -> do
        pb <- PB.pixbufNewFromFileAtSize  "./src/images/hidimg.png" 500 500
        pbCopy <- PB.pixbufCopy pb
        _ <- Gtk.imageSetFromPixbuf canvas pbCopy
        activateButtons builder
        return ()
    _-> do
        print "That file wasn't an image"
        return ()
  return ()



