{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Kestrel.hs file.
module Settings
    ( hamletFile
    , cassiusFile
    , juliusFile
    , widgetFile
    , connStr
    , ConnectionPool
    , withConnectionPool
    , runConnectionPool
    , approot
    , rootbase
    , staticroot
    , staticdir
    , s3dir
    , topTitle
    , sidePaneTitle
    , newDays
    , numOfRecentChanges
    , facebookApplicationId
    , facebookApplicationSecret
    , twitterConsumerKey
    , twitterConsumerSecret
    , googleAnalyticsUA
    , addThisUser
    , tz
    ) where

import qualified Text.Hamlet as H
import qualified Text.Cassius as H
import qualified Text.Lucius as H
import qualified Text.Julius as H
import Language.Haskell.TH.Syntax
import Database.Persist.Postgresql
import Yesod (MonadControlIO, addWidget, addCassius, addJulius)
import Data.Monoid (mempty)
import System.Directory (doesFileExist)
import Data.Text (Text)

-- | The base URL for your application. This will usually be different for
-- development and production. Yesod automatically constructs URLs for you,
-- so this value must be accurate to create valid links.
approot :: Text
#ifdef PRODUCTION
-- You probably want to change this. If your domain name was "yesod.com",
-- you would probably want it to be:
-- > approot = "http://www.yesod.com"
-- Please note that there is no trailing slash.
approot = "soubun.seitoku.ac.jp"
#else
approot = "soubun.seitoku.ac.jp"
#endif

rootbase :: Text
rootbase = "/kestrel"

-- | The location of static files on your system. This is a file system
-- path. The default value works properly with your scaffolded site.
staticdir :: FilePath
staticdir = "static"

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
staticroot :: Text
staticroot = "/static"


s3dir :: FilePath
s3dir = "s3"

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

facebookApplicationId,facebookApplicationSecret :: Text
(facebookApplicationId,facebookApplicationSecret) =
  ("196074110406072","e0d687d928a17ed6041ab822ac31868f")

twitterConsumerKey,twitterConsumerSecret :: Text
(twitterConsumerKey,twitterConsumerSecret) =
  ("djzqgcIRqpSThDh2QbeTw","DTri0466cHoEb3orbq9Y0JVh1e2TPtw83CcA1Flbsk")

googleAnalyticsUA :: Maybe Text
googleAnalyticsUA = Just "UA-22232523-1"

addThisUser :: Maybe Text
addThisUser = Just "bisocie"

-- | The database connection string. The meaning of this string is backend-
-- specific.
connStr :: Text
#ifdef PRODUCTION
connStr = "user=soubun password=sougoubunnka host=localhost port=5432 dbname=kestrel"
#else
connStr = "user=soubun password=sougoubunnka host=localhost port=5432 dbname=kestrel_debug"
#endif

-- | Your application will keep a connection pool and take connections from
-- there as necessary instead of continually creating new connections. This
-- value gives the maximum number of connections to be open at a given time.
-- If your application requests a connection when all connections are in
-- use, that request will fail. Try to choose a number that will work well
-- with the system resources available to you while providing enough
-- connections for your expected load.
--
-- Also, connections are returned to the pool as quickly as possible by
-- Yesod to avoid resource exhaustion. A connection is only considered in
-- use while within a call to runDB.
connectionCount :: Int
connectionCount = 10

-- The rest of this file contains settings which rarely need changing by a
-- user.

-- The following three functions are used for calling HTML, CSS and
-- Javascript templates from your Haskell code. During development,
-- the "Debug" versions of these functions are used so that changes to
-- the templates are immediately reflected in an already running
-- application. When making a production compile, the non-debug version
-- is used for increased performance.
--
-- You can see an example of how to call these functions in Handler/Root.hs
--
-- Note: due to polymorphic Hamlet templates, hamletFileDebug is no longer
-- used; to get the same auto-loading effect, it is recommended that you
-- use the devel server.

toHamletFile, toCassiusFile, toJuliusFile, toLuciusFile :: String -> FilePath
toHamletFile x = "hamlet/" ++ x ++ ".hamlet"
toCassiusFile x = "cassius/" ++ x ++ ".cassius"
toJuliusFile x = "julius/" ++ x ++ ".julius"
toLuciusFile x = "lucius/" ++ x ++ ".lucius"

hamletFile :: FilePath -> Q Exp
hamletFile = H.hamletFile . toHamletFile

cassiusFile :: FilePath -> Q Exp
#ifdef PRODUCTION
cassiusFile = H.cassiusFile . toCassiusFile
#else
cassiusFile = H.cassiusFileDebug . toCassiusFile
#endif

luciusFile :: FilePath -> Q Exp
#ifdef PRODUCTION
luciusFile = H.luciusFile . toLuciusFile
#else
luciusFile = H.luciusFileDebug . toLuciusFile
#endif

juliusFile :: FilePath -> Q Exp
#ifdef PRODUCTION
juliusFile = H.juliusFile . toJuliusFile
#else
juliusFile = H.juliusFileDebug . toJuliusFile
#endif

widgetFile :: FilePath -> Q Exp
widgetFile x = do
    let h = unlessExists toHamletFile hamletFile
    let c = unlessExists toCassiusFile cassiusFile
    let j = unlessExists toJuliusFile juliusFile
    [|addWidget $h >> addCassius $c >> addJulius $j|]
  where
    unlessExists tofn f = do
        e <- qRunIO $ doesFileExist $ tofn x
        if e then f x else [|mempty|]

-- The next two functions are for allocating a connection pool and running
-- database actions using a pool, respectively. It is used internally
-- by the scaffolded application, and therefore you will rarely need to use
-- them yourself.
withConnectionPool :: MonadControlIO m => (ConnectionPool -> m a) -> m a
withConnectionPool = withPostgresqlPool connStr connectionCount

runConnectionPool :: MonadControlIO m => SqlPersist m a -> ConnectionPool -> m a
runConnectionPool = runSqlPool
