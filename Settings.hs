-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings where

import ClassyPrelude.Yesod
import Control.Exception           (throw)
import Data.Aeson                  (Result (..), fromJSON, withObject, (.!=),
                                    (.:?))
import Data.FileEmbed              (embedFile)
import Data.Yaml                   (decodeEither')
import Network.Wai.Handler.Warp    (HostPreference)
import Yesod.Default.Config2       (applyEnvValue, configSettingsYml)
import Yesod.Default.Util          (WidgetFileSettings, widgetFileNoReload,
                                    widgetFileReload)

import Language.Haskell.TH.Syntax
import Database.Persist.Postgresql (PostgresConf)
import qualified Data.ByteString.Char8 as SB
import "crypto-pubkey" Crypto.PubKey.RSA


-- | Runtime settings to configure this application. These settings can be
-- loaded from various sources: defaults, environment variables, config files,
-- theoretically even a database.
data AppSettings = AppSettings
    { appStaticDir              :: String
    -- ^ Directory from which to serve static files.
    , appDatabaseConf           :: PostgresConf
    -- ^ Configuration settings for accessing the database.
    , appRoot                   :: Text
    -- ^ Base for all generated URLs.
    , appHost                   :: HostPreference
    -- ^ Host/interface the server should bind to.
    , appPort                   :: Int
    -- ^ Port to listen on
    , appIpFromHeader           :: Bool
    -- ^ Get the IP address from the header when logging. Useful when sitting
    -- behind a reverse proxy.

    , appDetailedRequestLogging :: Bool
    -- ^ Use detailed request logging system
    , appShouldLogAll           :: Bool
    -- ^ Should all log messages be displayed?
    , appReloadTemplates        :: Bool
    -- ^ Use the reload version of templates
    , appMutableStatic          :: Bool
    -- ^ Assume that files in the static dir may change after compilation
    , appSkipCombining          :: Bool
    -- ^ Perform no stylesheet/script combining

    -- Example app-specific configuration values.
    , appCopyright              :: Text
    -- ^ Copyright text to appear in the footer of the page
    , appAnalytics              :: Maybe Text
    -- ^ Google Analytics code
    }


instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        let defaultDev =
#if DEVELOPMENT
                True
#else
                False
#endif
        appStaticDir              <- o .: "static-dir"
        appDatabaseConf           <- o .: "database"
        appRoot                   <- o .: "approot"
        appHost                   <- fromString <$> o .: "host"
        appPort                   <- o .: "port"
        appIpFromHeader           <- o .: "ip-from-header"

        appDetailedRequestLogging <- o .:? "detailed-logging" .!= defaultDev
        appShouldLogAll           <- o .:? "should-log-all"   .!= defaultDev
        appReloadTemplates        <- o .:? "reload-templates" .!= defaultDev
        appMutableStatic          <- o .:? "mutable-static"   .!= defaultDev
        appSkipCombining          <- o .:? "skip-combining"   .!= defaultDev

        appCopyright              <- o .: "copyright"
        appAnalytics              <- o .:? "analytics"

        return AppSettings {..}

-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
--
-- For more information on modifying behavior, see:
--
-- https://github.com/yesodweb/yesod/wiki/Overriding-widgetFile
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def

-- | How static files should be combined.
combineSettings :: CombineSettings
combineSettings = def

-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: String -> Q Exp
widgetFile = (if appReloadTemplates compileTimeAppSettings
                then widgetFileReload
                else widgetFileNoReload)
              widgetFileSettings

-- | Raw bytes at compile time of @config/settings.yml@
configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml)

-- | @config/settings.yml@, parsed to a @Value@.
configSettingsYmlValue :: Value
configSettingsYmlValue = either throw id $ decodeEither' configSettingsYmlBS

-- | A version of @AppSettings@ parsed at compile time from @config/settings.yml@.
compileTimeAppSettings :: AppSettings
compileTimeAppSettings =
    case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
        Error e -> error e
        Success settings -> settings

-- The following two functions can be used to combine multiple CSS or JS files
-- at compile time to decrease the number of http requests.
-- Sample usage (inside a Widget):
--
-- > $(combineStylesheets 'StaticR [style1_css, style2_css])

combineStylesheets :: Name -> [Route Static] -> Q Exp
combineStylesheets = combineStylesheets'
    (appSkipCombining compileTimeAppSettings)
    combineSettings

combineScripts :: Name -> [Route Static] -> Q Exp
combineScripts = combineScripts'
    (appSkipCombining compileTimeAppSettings)
    combineSettings

-- The rest of this file contains Kestrel's settings

s3dir :: FilePath
s3dir = "s3"

s3ThumbnailDir :: FilePath
s3ThumbnailDir = "s3/_thumbnail"

topTitle :: Text
topTitle = "Kestrel"
sidePaneTitle :: Text
sidePaneTitle = "Navi"

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
owl_pass_service_url :: String
owl_pass_service_url = "http://localhost:3002/srv/change-pass"

-- |
-- Kestrel RSA keys
--
clientId :: SB.ByteString
clientId = "Kestrel"

kestrel_pub :: PublicKey
kestrel_pub = PublicKey { public_size = 256
                        , public_n = 21228709312220288517503495190383087008620565254339967134095466068365647031226729669265671664798784928948846353620011623578414876424990762980943729885946652556572428902689997212044632493925435778543200901607282978155864935426522893231195315858378754230462066295557643431595879074834502607097823427913242585518657629041668450361956795113827730227717621313907040381667287023411039162112335885551742896477496864860480202946704420444852614551591081039685989010619581744204539847080564272228683094296009683827755542701376148992554748322533792248770505527486231377342851844922810933440422738171377892597476265198722904705743
                        , public_e = 65537
                        }
              
kestrel_priv :: PrivateKey
kestrel_priv = PrivateKey { private_pub =
                               PublicKey { public_size = 256
                                         , public_n = 21228709312220288517503495190383087008620565254339967134095466068365647031226729669265671664798784928948846353620011623578414876424990762980943729885946652556572428902689997212044632493925435778543200901607282978155864935426522893231195315858378754230462066295557643431595879074834502607097823427913242585518657629041668450361956795113827730227717621313907040381667287023411039162112335885551742896477496864860480202946704420444852614551591081039685989010619581744204539847080564272228683094296009683827755542701376148992554748322533792248770505527486231377342851844922810933440422738171377892597476265198722904705743
                                         , public_e = 65537
                                         }
                          , private_d = 5161980432420503193843717279612201879386870437968807181423399717189907244604256588025355808935920726120036246567412381301335420765501209986180619794351982164907429802909315281003757624291556595005332095884975838684893474082687166433195424775609561429675503738132758681750948760800198873105435313597287545093926656215512502535142427442601664094818330146872263255296146899655055106505513450826075796283989835677278409679726632862330182061587031862314693744531682575836449305897907667325702545545667370145365650302464141436234985856174385530390917398927182802403784331616885954597326285769033313538551708379274790205953
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
owl_pub = PublicKey { public_size = 256
                    , public_n = 23935502722801508291122222398018117881284958223263854065673689606867055652122077115632498984650750679970467900697728966520426415008444072251453446123881488809248692462117519335720631061157343736650249371835293662619945999329307142886808914215692490190245599500864907497806854772652186075160282343362861100625964817657470875052275949634580109631117392627776939182328215081842240646543745078419135398375800047086393491931547537516953037019818981085723402984601825491050312705896863144307436654552505557222743591857763940190952404403348742192979262305085887506928609325609473826220183742944601830381993567783603917096371
                    , public_e = 65537
                    }


facebookApplicationName,facebookApplicationId,facebookApplicationSecret :: ByteString
(facebookApplicationName,facebookApplicationId,facebookApplicationSecret) =
  ("kestrel.org" :: ByteString,"196074110406072" :: ByteString,"e0d687d928a17ed6041ab822ac31868f" :: ByteString)

twitterConsumerKey,twitterConsumerSecret :: Text
(twitterConsumerKey,twitterConsumerSecret) =
  ("djzqgcIRqpSThDh2QbeTw" :: Text,"DTri0466cHoEb3orbq9Y0JVh1e2TPtw83CcA1Flbsk" :: Text)

googleAnalyticsUA :: Maybe Text
googleAnalyticsUA = Just "UA-2279508-4"

addThisUser :: Maybe Text
addThisUser = Just "cutsea110"
