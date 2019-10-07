module CssStyle where

import qualified Data.ByteString
import qualified GI.Gdk
import qualified GI.Gtk

data GuiObjects = GuiObjects { window :: GI.Gtk.Window}

applyCss :: GuiObjects -> IO ()
applyCss guiObjects = do
  maybeScreen         <- GI.Gdk.screenGetDefault
  provider            <- GI.Gtk.cssProviderNew
  styleFile           <- return "./src/style/style.css"
  case maybeScreen of
    (Just screen) -> do
      styleFileContents    <- Data.ByteString.readFile styleFile
      GI.Gtk.cssProviderLoadFromData provider styleFileContents
      GI.Gtk.styleContextAddProviderForScreen screen provider (fromIntegral GI.Gtk.STYLE_PROVIDER_PRIORITY_USER)
    _ -> return ()



