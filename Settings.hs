{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Kestrel.hs file.
module Settings
    ( widgetFile
    , PersistConfig
    , staticRoot
    , staticDir
    , Extra (..)
    , parseExtra
    , s3dir
    , s3ThumbnailDir
    , topTitle
    , sidePaneTitle
    , newDays
    , numOfRecentChanges
    , facebookApplicationName
    , facebookApplicationId
    , facebookApplicationSecret
    , twitterConsumerKey
    , twitterConsumerSecret
    , googleAnalyticsUA
    , addThisUser
    , tz
    ) where

import Prelude
import Text.Shakespeare.Text (st)
import Language.Haskell.TH.Syntax
import Database.Persist.Postgresql (PostgresConf)
import Yesod.Default.Config
import qualified Yesod.Default.Util
import Data.Text (Text)
import Network.HTTP.Types (Ascii)
import Data.Yaml
import Control.Applicative

-- | Which Persistent backend this site is using.
type PersistConfig = PostgresConf

-- Static setting below. Changing these requires a recompile

-- | The location of static files on your system. This is a file system
-- path. The default value works properly with your scaffolded site.
staticDir :: FilePath
staticDir = "static"

-- | The base URL for your static files. As you can see by the default
-- value, this can simply be "static" appended to your application root.
-- A powerful optimization can be serving static files from a separate
-- domain name. This allows you to use a web server optimized for static
-- files, more easily set expires and cache values, and avoid possibly
-- costly transference of cookies on static files. For more information,
-- please see:
--   http://code.google.com/speed/page-speed/docs/request.html#ServeFromCookielessDomain
--
-- If you change the resource pattern for StaticR in Kestrel.hs, you will
-- have to make a corresponding change here.
--
-- To see how this value is used, see urlRenderOverride in Kestrel.hs
staticRoot :: AppConfig DefaultEnv x -> Text
staticRoot conf = [st|#{appRoot conf}/static|]

-- The rest of this file contains settings which rarely need changing by a
-- user.


widgetFile :: FilePath -> Q Exp
#if DEVELOPMENT
widgetFile = Yesod.Default.Util.widgetFileReload
#else
widgetFile = Yesod.Default.Util.widgetFileNoReload
#endif

data Extra = Extra
    { extraCopyright :: Text
    , extraAnalytics :: Maybe Text -- ^ Google Analytics
    } deriving Show

parseExtra :: DefaultEnv -> Object -> Parser Extra
parseExtra _ o = Extra
    <$> o .:  "copyright"
    <*> o .:? "analytics"

s3dir :: FilePath
s3dir = "s3"

s3ThumbnailDir :: FilePath
s3ThumbnailDir = "s3/_thumbnail"

topTitle :: Text
topTitle = "聖徳大学短期大学部総合文化学科"
sidePaneTitle :: Text
sidePaneTitle = "サイト ナビ"

newDays :: Integer
newDays = 3
numOfRecentChanges :: Int
numOfRecentChanges = 20

-- | TimeZone. this value used as `hoursToTimeZone tz'.
tz :: Int
tz = 9


facebookApplicationName,facebookApplicationId,facebookApplicationSecret :: Ascii
(facebookApplicationName,facebookApplicationId,facebookApplicationSecret) =
  ("kestrel.org","196074110406072","e0d687d928a17ed6041ab822ac31868f")

twitterConsumerKey,twitterConsumerSecret :: Text
(twitterConsumerKey,twitterConsumerSecret) =
  ("djzqgcIRqpSThDh2QbeTw","DTri0466cHoEb3orbq9Y0JVh1e2TPtw83CcA1Flbsk")

googleAnalyticsUA :: Maybe Text
googleAnalyticsUA = Just "UA-22232523-1"

addThisUser :: Maybe Text
addThisUser = Just "bisocie"
