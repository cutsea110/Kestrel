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
      -- for Owl service
    , clientId
    , kestrel_pub
    , kestrel_priv
    , owl_pub
    , owl_auth_service_url
    ) where

import Text.Shakespeare.Text (st)
import Language.Haskell.TH.Syntax
import Database.Persist.Postgresql (PostgresConf)
import Yesod.Default.Config
import Yesod.Default.Util
import Data.Text (Text)
import Data.ByteString
import qualified Data.ByteString.Char8 as SB
import Data.Yaml
import Control.Applicative
import Settings.Development
import Data.Default (def)
import Text.Hamlet
import Crypto.PubKey.RSA

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

-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def
    { wfsHamletSettings = defaultHamletSettings
        { hamletNewlines = AlwaysNewlines
        }
    }

-- The rest of this file contains settings which rarely need changing by a
-- user.
widgetFile :: FilePath -> Q Exp
widgetFile = (if development 
              then widgetFileReload 
              else widgetFileNoReload)
             widgetFileSettings

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


-- |
-- Owl service URL
--
owl_auth_service_url :: String
owl_auth_service_url = "http://localhost:3002/srv/auth"

-- |
-- Kestrel RSA keys
--
clientId :: SB.ByteString
clientId = "Kestrel"

kestrel_pub :: PublicKey
kestrel_pub = PublicKey 
               { public_size = 128
               , public_n = 120332330775436211464295534571972888673387300679598566062508608458655719305789686349395492378128543547366411787689981100009887893124585139574893280404868432860950478019280547842280406034811001082141945533376685014426870049436530317211335145209888151263557448528262054842217958692127741265705082667427881950147
               , public_e = 65537
               }
              
kestrel_priv :: PrivateKey
kestrel_priv = PrivateKey 
                { private_pub = 
                     PublicKey { public_size = 128
                               , public_n = 120332330775436211464295534571972888673387300679598566062508608458655719305789686349395492378128543547366411787689981100009887893124585139574893280404868432860950478019280547842280406034811001082141945533376685014426870049436530317211335145209888151263557448528262054842217958692127741265705082667427881950147
                               , public_e = 65537
                               }
                , private_d = 108913639576686836315960183098711991856935592134403890042203421526016448079418231149937315056927063279409811471721230738214848559506296310623060555213939978879566824325909705360013708254754281175222687688422959488634993437769671023073865224805150410126139061626691044320259957432757783646431176759950139246833
                , private_p = 0
                , private_q = 0
                , private_dP = 0
                , private_dQ = 0
                , private_qinv = 0
                }

-- |
-- Owl RSA keys
--
owl_pub :: PublicKey
owl_pub = PublicKey { public_size = 128
                    , public_n = 133978865898371049756915690541771190310631304559640804303990893481160872232160722925370093358396509346866547508177130752551249861802825991982314077620630462699557927940806588373415331051489847062718976316744747135498419296507215040001805779727816051742538971179969585665983463554641712741262022247106195741053
                    , public_e = 65537
                    }


facebookApplicationName,facebookApplicationId,facebookApplicationSecret :: ByteString
(facebookApplicationName,facebookApplicationId,facebookApplicationSecret) =
  ("kestrel.org","196074110406072","e0d687d928a17ed6041ab822ac31868f")

twitterConsumerKey,twitterConsumerSecret :: Text
(twitterConsumerKey,twitterConsumerSecret) =
  ("djzqgcIRqpSThDh2QbeTw","DTri0466cHoEb3orbq9Y0JVh1e2TPtw83CcA1Flbsk")

googleAnalyticsUA :: Maybe Text
googleAnalyticsUA = Just "UA-22232523-1"

addThisUser :: Maybe Text
addThisUser = Just "bisocie"
