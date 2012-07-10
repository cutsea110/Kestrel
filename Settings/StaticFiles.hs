{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Settings.StaticFiles where

import Prelude (IO)
import Yesod.Static
import qualified Yesod.Static as Static
import Settings (staticDir)

-- | use this to create your static file serving site
staticSite :: IO Static.Static
staticSite =
#if DEVELOPMENT
  Static.staticDevel staticDir
#else
  Static.static staticDir
#endif

$(staticFiles Settings.staticDir)
