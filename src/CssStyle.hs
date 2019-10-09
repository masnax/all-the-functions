module CssStyle where

import qualified Data.ByteString
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk

data GuiObjects = GuiObjects { window :: Gtk.Window}

applyCss :: GuiObjects -> IO ()
applyCss guiObjects = do
  maybeScreen         <- Gdk.screenGetDefault
  provider            <- Gtk.cssProviderNew
  styleFile           <- return "./src/style/style.css"
  case maybeScreen of
    (Just screen) -> do
      styleFileContents    <- Data.ByteString.readFile styleFile
      Gtk.cssProviderLoadFromData provider styleFileContents
      Gtk.styleContextAddProviderForScreen screen provider (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER)
    _ -> return ()



