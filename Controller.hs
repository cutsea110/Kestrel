{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Controller
    ( withKestrel
    ) where

import Kestrel
import Settings
import Yesod.Helpers.Static
import Yesod.Helpers.Auth
import Database.Persist.GenericSql

-- Import all relevant handler modules here.
import Handler.Root
import Handler.Wiki
import Handler.YS3

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Kestrel.hs. Please see
-- the comments there for more details.
mkYesodDispatch "Kestrel" resourcesKestrel

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
withKestrel :: (Application -> IO a) -> IO a
withKestrel f = Settings.withConnectionPool $ \p -> do
    runConnectionPool (runMigration migrateAll) p
    let h = Kestrel s p
    toWaiApp h >>= f
  where
    s = fileLookupDir Settings.staticdir typeByExt
