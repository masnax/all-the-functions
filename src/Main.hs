{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module Main where

import Data.Text
import qualified GI.Gtk as Gtk
import Data.GI.Base

import qualified CssStyle as CSS
import Dialogue
import Window
import Button

{-
responsible for creating and destroying the window
Upon creation, creates listeners for buttons
-}
main :: IO ()
main = do
    Gtk.init Nothing
    builder <- Gtk.builderNewFromFile (pack "./src/style/gui.glade")
    window <- getGTKWidget Gtk.Window "window" builder
    importButton <- getGTKWidget  Gtk.Button "import" builder
    exportButton <- getGTKWidget  Gtk.Button "export" builder
    canvas <- getGTKWidget Gtk.Image "canvas" builder
    Gtk.windowSetResizable window False
    let guiObjects = CSS.GuiObjects { CSS.window = window}

    on importButton #clicked $ selectFile builder
    on exportButton #clicked $ exportFile builder
    setButtonActive "export" False builder
    initializeButtons builder
    setConvolutionsActive False builder
    on window #destroy Gtk.mainQuit
    CSS.applyCss guiObjects
    Gtk.widgetShowAll window
    Gtk.main
