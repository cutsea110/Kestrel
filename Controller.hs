{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Controller
    ( withKestrel
    , withDevelApp
    ) where

import Kestrel
import Settings
import Yesod.Static
import Yesod.Auth
import Database.Persist.GenericSql
import Network.Wai
import Data.Dynamic (Dynamic, toDyn)

-- Import all relevant handler modules here.
import Handler.Root
import Handler.Profile
import Handler.Wiki
import Handler.S3

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
    s' <- s
    http <- toWaiApp $ Kestrel s' p False
    https <- toWaiApp $ Kestrel s' p True
    f $ \req -> (if isSecure req then https else http) req
  where
    s = static Settings.staticdir

withDevelApp :: Dynamic
withDevelApp = toDyn (withKestrel :: (Application -> IO ()) -> IO ())
