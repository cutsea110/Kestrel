{-# OPTIONS_GHC -fspec-constr-count=100 #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Foundation where

import Import.NoFoundation as Import hiding (last)
import Text.Hamlet (ihamletFile)
import Text.Lucius (luciusFile)
import Text.Julius (juliusFile)
import Text.Jasmine (minifym)
import Yesod.Auth.Message   (AuthMessage (InvalidLogin))
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe

import Database.Persist.Sql
import Text.Julius (RawJS(..))
import Text.Pandoc
import Text.Pandoc.Shared
import qualified Data.Map as Map (lookup, fromList, Map)
import Data.List (inits, last)
import qualified Data.Text as T
import Text.Blaze.Internal (preEscapedString)

import Yesod.AtomFeed
import Yesod.Auth.Owl
import Yesod.Form.Jquery
import Yesod.Goodies.PNotify

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

instance HasHttpManager App where
    getHttpManager = appHttpManager

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)


(+++) :: Text -> Text -> Text
(+++) = T.append

newtype WikiPage = WikiPage { unWikiPage :: [Text] } deriving (Eq, Show, Read)
instance PathMultiPiece WikiPage where
  toPathMultiPiece = unWikiPage
  fromPathMultiPiece = Just . WikiPage
  
topPage :: WikiPage
topPage = WikiPage [Import.topTitle]
topView :: (Route App, [(Text, Text)])
topView = (WikiR topPage, [("mode","v")])
topNew :: (Route App, [(Text, Text)])
topNew = (NewR, [("path", Import.topTitle)])

sidePane :: WikiPage
sidePane = WikiPage [Import.sidePaneTitle]
sidePaneView :: Route App
sidePaneView = WikiR sidePane
sidePaneNew :: (Route App, [(Text, Text)])
sidePaneNew = (NewR, [("path", Import.sidePaneTitle), ("mode", "e")])
simpleSidePane :: (Route App, [(Text, Text)])
simpleSidePane = (WikiR sidePane, [("mode", "s")])
isSidePane :: Text -> Bool
isSidePane p = Import.sidePaneTitle == p

pathOf :: WikiPage -> Text
pathOf = T.intercalate ":" . unWikiPage

fromPath :: Text -> WikiPage
fromPath path = WikiPage $ T.splitOn ":" path

fromWiki :: Wiki -> WikiPage
fromWiki = fromPath . wikiPath

lastNameOf :: WikiPage -> Text
lastNameOf = last . unWikiPage

ancestory :: WikiPage -> [WikiPage]
ancestory = map WikiPage . filter (/=[]) . inits . unWikiPage

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . appSettings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120 -- timeout in minutes
        "config/client_session_key.aes"
    
    defaultLayout widget = do
        y <- getYesod
        mu <- maybeAuth
        cr <- getCurrentRoute
        msgShow <- getMessageRender
        let mgaUA = Import.googleAnalyticsUA
            ga = $(ihamletFile "templates/ga.hamlet")
            header = $(ihamletFile "templates/header.hamlet")
            footer = $(ihamletFile "templates/footer.hamlet")
        mlastup <- runDB $ selectFirst [WikiTouched !=. Nothing] [Desc WikiTouched,LimitTo 1]
        pc <- widgetToPageContent $ do
          widget
          pnotify y
          addScriptEither $ urlJqueryJs y
          addScriptEither $ urlJqueryUiJs y
          addStylesheetEither $ urlJqueryUiCss y
          addScriptEither $ Left $ StaticR plugins_upload_jquery_upload_1_0_2_js
          addScriptEither $ Left $ StaticR plugins_bubbleup_jquery_bubbleup_js
          addScriptEither $ Left $ StaticR plugins_exinplaceeditor_jquery_exinplaceeditor_0_1_3_js
          addStylesheetEither $ Left $ StaticR plugins_exinplaceeditor_exinplaceeditor_css
          addScriptEither $ Left $ StaticR plugins_watermark_jquery_watermark_js
          toWidget $(luciusFile "templates/default-layout.lucius")
          toWidget $(juliusFile "templates/default-layout.julius")
          toWidget $(luciusFile "templates/leftnavi.lucius")
          atomLink FeedR Import.topTitle
        ihamletToHtml $(ihamletFile "templates/default-layout.hamlet")
        
    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR
    
    -- Maximum allowed length of the request body, in bytes.
    maximumContentLength _ (Just UploadR)     = Just (20 * 1024 * 1024) -- 20 megabytes
    maximumContentLength _ (Just (FileR _ _)) = Just (20 * 1024 * 1024) -- 20 megabytes
    maximumContentLength _ _                  = Just (2 * 1024 * 1024) --  2 megabytes for default
    
    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = RootR
    -- Where to send a user after logout
    logoutDest _ = RootR

    authenticate creds = do
      render <- getMessageRender
      runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid u) ->
              if userActive u
              then do
                lift $ setPNotify $ PNotify JqueryUI Success "Login" $ render MsgNowLogin
                return $ Authenticated uid
              else do
                lift $ setPNotify $ PNotify JqueryUI Error "Login failed" $ render MsgInvalidAccount
                return $ UserError InvalidLogin
            Nothing -> do
              lift $ setPNotify $ PNotify JqueryUI Success "Login" $ render MsgNowLogin
              fmap Authenticated $ insert $ User (credsIdent creds) Nothing Nothing True

    authPlugins _ = [ authOwl
                    ]
    
    authHttpManager = getHttpManager

instance YesodAuthPersist App

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

instance YesodAuthOwl App where
  getOwlIdent = lift $ fmap (userIdent . entityVal) requireAuth
  clientId _ = Import.clientId
  owlPubkey _ = Import.owl_pub
  myPrivkey _ = Import.kestrel_priv
  endpoint_auth _ = Import.owl_auth_service_url
  endpoint_pass _ = Import.owl_pass_service_url

instance YesodJquery App where
  urlJqueryJs _ = Left $ StaticR js_jquery_1_4_4_min_js
  urlJqueryUiJs _ = Left $ StaticR js_jquery_ui_1_8_9_custom_min_js
  urlJqueryUiCss _ = Left $ StaticR css_jquery_ui_1_8_9_custom_css

instance YesodJqueryPnotify App where

{- markdown utility -}
markdownToWikiHtml opt raw = do
  render <- lift getUrlRenderParams
  pages <- selectList [] [Asc WikiPath, Desc WikiUpdated]
  let pandoc = readDoc raw
  let pdict = mkWikiDictionary pages
  return $ preEscapedString $ writeHtmlStr opt render pdict $ pandoc

markdownsToWikiHtmls opt raws = do
  render <- lift getUrlRenderParams
  pages <- selectList [] [Asc WikiPath, Desc WikiUpdated]
  let pandocs = map readDoc raws
  let pdict = mkWikiDictionary pages
  return $ map (preEscapedString . writeHtmlStr opt render pdict) pandocs

readDoc :: Text -> Pandoc
readDoc t = let Right p = readMarkdown def $ tabFilter (readerTabStop def) $ T.unpack t
            in p

wikiWriterOption :: (AppMessage -> Text) -> WriterOptions
wikiWriterOption msgShow =
  def { writerStandalone = True
      , writerTemplate = "$if(toc)$\n<a id='pandoc-TOC-toggle' href=''></a><div id='pandoc-TOC-Title'>" ++ T.unpack (msgShow MsgTOC) ++ "$toc$</div>\n$endif$\n$body$"
      , writerTableOfContents = False
      , writerNumberSections = False
      , writerIdentifierPrefix = "pandoc-"
      }

sidePaneWriterOption :: WriterOptions
sidePaneWriterOption = 
  def { writerStandalone = True
      , writerTemplate = "$body$"
      , writerTableOfContents = False
      , writerNumberSections = False
      , writerIdentifierPrefix = "sidepane-"
      }

writeHtmlStr ::  WriterOptions -> (Route App -> [(Text, Text)] -> Text) -> Map.Map Text Wiki -> Pandoc -> String
writeHtmlStr opt render pages = 
  writeHtmlString opt . transformDoc render pages

transformDoc :: (Route App -> [(Text, Text)] -> Text) -> Map.Map Text Wiki -> Pandoc -> Pandoc
transformDoc render pages = bottomUp (wikiLink render pages)

-- Wiki Link Sign of WikiName is written as [WikiName]().
wikiLink :: (Route App -> [(Text, Text)] -> Text) -> Map.Map Text Wiki -> Inline -> Inline
wikiLink render pages (Link ls ("", "")) = 
  case Map.lookup path' pages of
    Just _  -> 
      Link [Str p''] (render' (WikiR $ fromPath path') [("mode", "v")], p')
    Nothing -> 
      Emph [Str p'', Link [Str "?"] (render' NewR [("path", path'), ("mode", "v")], p')]
  where
    p' = inlinesToString ls
    p'' = lastOf "" p'
    path' = T.pack p'
    render' = (T.unpack .) . render
    lastOf r [] = r
    lastOf _ (':':xs) = lastOf "" xs
    lastOf r (x:xs) = lastOf (r ++ [x]) xs
wikiLink _ _ x = x

mkWikiDictionary :: [Entity Wiki] -> Map.Map Text Wiki
mkWikiDictionary = Map.fromList . map (((,).wikiPath.entityVal) <*> entityVal)

-- Network.Gitit.ContentTransformer
inlinesToString :: [Inline] -> String
inlinesToString = concatMap go
  where go x = case x of
          Str s                   -> s
          Emph xs                 -> concatMap go xs
          Strong xs               -> concatMap go xs
          Strikeout xs            -> concatMap go xs
          Superscript xs          -> concatMap go xs
          Subscript xs            -> concatMap go xs
          SmallCaps xs            -> concatMap go xs
          Quoted DoubleQuote xs   -> '"' : (concatMap go xs ++ "\"")
          Quoted SingleQuote xs   -> '\'' : (concatMap go xs ++ "'")
          Cite _ xs               -> concatMap go xs
          Code _ s                -> s
          Space                   -> " "
          LineBreak               -> " "
          Math DisplayMath s      -> "$$" ++ s ++ "$$"
          Math InlineMath s       -> "$" ++ s ++ "$"
          RawInline "tex" s       -> s
          RawInline _ _           -> ""
          Link xs _               -> concatMap go xs
          Image xs _              -> concatMap go xs
          Note _                  -> ""
          Span _ xs               -> concatMap go xs

-- TODO: remove this if yesod support Root Relative URL.
dropPrefix :: Text -> Text -> Text
dropPrefix xs ys = dp' ys xs ys
  where
    dp' o x y | T.null x = y
              | T.null y = o
              | T.head x == T.head y = dp' o (T.tail x) (T.tail y)
              | otherwise = o

dropSchema :: Text -> Text
dropSchema s | s `T.isPrefixOf` "http://" = T.drop 7 s
             | s `T.isPrefixOf` "https://" = T.drop 8 s
             | otherwise = s -- FIXME
