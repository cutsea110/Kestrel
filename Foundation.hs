{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fspec-constr-count=100 #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Foundation
    ( App (..)
    , Route (..)
    , AppMessage (..)
    , resourcesApp
    , Handler
    , Widget
    , maybeAuth
    , requireAuth
    , module Settings
    , module Model
      -- 
    , WikiPage(..)
    , topPage
    , topNew
    , topView
    , sidePane
    , sidePaneNew
    , simpleSidePane
    , isSidePane
    , lastNameOf
    , pathOf
    , fromPath
    , fromWiki
    , ancestory
    , setpassR -- Auth.HashDB
      --
    , markdownToWikiHtml
    , markdownsToWikiHtmls
    , wikiWriterOption
    , sidePaneWriterOption
    , WriterOptions(..)
    , dropPrefix
      --
    , (+++)
    ) where

import Yesod
import Yesod.Static
import Settings.StaticFiles
import Yesod.AtomFeed
import Yesod.Auth
import Kestrel.Helpers.Auth.HashDB
import Yesod.Auth.GoogleEmail
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Network.HTTP.Conduit (Manager)
import qualified Settings
import qualified Database.Persist.Store
import Database.Persist.GenericSql
import Settings (widgetFile, Extra (..))
import Model
import Text.Jasmine (minifym)
import Web.ClientSession (getKey)
import Text.Hamlet (ihamletFile)
import Text.Lucius (luciusFile)
import Text.Julius (juliusFile)
import Yesod.Form.Jquery
import Control.Applicative ((<*>))
import Text.Pandoc
import Text.Pandoc.Shared
import qualified Text.Pandoc.Highlighting as PH (formatHtmlBlock, highlight)
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Data.Map as Map (lookup, fromList, Map)
import Data.List (inits)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Time.Clock (UTCTime(..))
import qualified Data.Text as T
import Text.Blaze.Internal (preEscapedText, preEscapedString)

(+++) :: Text -> Text -> Text
(+++) = T.append

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.Store.PersistConfigPool Settings.PersistConfig -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConfig
    }

mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://docs.yesodweb.com/book/web-routes-quasi/
--
-- This function does three things:
--
-- * Creates the route datatype AppRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route App = AppRoute
-- * Creates the value resourcesApp which contains information on the
--   resources declared below. This is used in Application.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- App. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the AppRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm App App (FormResult x, Widget)

newtype WikiPage = WikiPage { unWikiPage :: [Text] } deriving (Eq, Show, Read)
instance PathMultiPiece WikiPage where
  toPathMultiPiece = unWikiPage
  fromPathMultiPiece = Just . WikiPage
  
topPage :: WikiPage
topPage = WikiPage [Settings.topTitle]
topView :: (Route App, [(Text, Text)])
topView = (WikiR topPage, [("mode","v")])
topNew :: (Route App, [(Text, Text)])
topNew = (NewR, [("path", Settings.topTitle)])

sidePane :: WikiPage
sidePane = WikiPage [Settings.sidePaneTitle]
sidePaneView :: Route App
sidePaneView = WikiR sidePane
sidePaneNew :: (Route App, [(Text, Text)])
sidePaneNew = (NewR, [("path", Settings.sidePaneTitle), ("mode", "e")])
simpleSidePane :: (Route App, [(Text, Text)])
simpleSidePane = (WikiR sidePane, [("mode", "s")])
isSidePane :: Text -> Bool
isSidePane p = Settings.sidePaneTitle == p

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
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = do
        key <- getKey "config/client_session_key.aes"
        return . Just $ clientSessionBackend key 120
    
    defaultLayout widget = do
        y <- getYesod
        mu <- maybeAuth
        mmsg <- getMessage
        r2m <- getRouteToMaster
        cr <- getCurrentRoute
        msgShow <- getMessageRender
        let mgaUA = Settings.googleAnalyticsUA
            maTUser = Settings.addThisUser
            (ApprootMaster approot') = approot
            googleInurl = dropSchema $ approot' y 
            -- dropSchema $ appRoot $ settings y -- by approot's defninition
            ga = $(ihamletFile "templates/ga.hamlet")
            header = $(ihamletFile "templates/header.hamlet")
            footer = $(ihamletFile "templates/footer.hamlet")
        mlastup <- runDB $ selectFirst [WikiTouched !=. Nothing] [Desc WikiTouched,LimitTo 1]
        pc <- widgetToPageContent $ do
          widget
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
          atomLink FeedR Settings.topTitle
        ihamletToRepHtml $(ihamletFile "templates/default-layout.hamlet")
        
    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticroot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR
    
    -- Maximum allowed length of the request body, in bytes.
    maximumContentLength _ (Just UploadR)     = 20 * 1024 * 1024 -- 20 megabytes
    maximumContentLength _ (Just (FileR _ _)) = 20 * 1024 * 1024 -- 20 megabytes
    maximumContentLength _ _                  =  2 * 1024 * 1024 --  2 megabytes for default
    
    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent = addStaticContentExternal minifym base64md5 Settings.staticDir (StaticR . flip StaticRoute [])

    -- Enable Javascript async loading
--    yepnopeJs _ = Just $ Right $ StaticR js_modernizr_js

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersist
    runDB f = do
        master <- getYesod
        Database.Persist.Store.runPool
            (persistConfig master)
            f
            (connPool master)
    
instance YesodJquery App where
  urlJqueryJs _ = Left $ StaticR js_jquery_1_4_4_min_js
  urlJqueryUiJs _ = Left $ StaticR js_jquery_ui_1_8_9_custom_min_js
  urlJqueryUiCss _ = Left $ StaticR css_jquery_ui_1_8_9_custom_css

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = RootR
    -- Where to send a user after logout
    logoutDest _ = RootR

    getAuthId creds = do
      msgShow <- getMessageRender
      runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid u) -> 
              if userActive u 
              then do
                lift $ setMessage $ preEscapedText $ msgShow MsgNowLogin
                return $ Just uid 
              else do
                lift $ setMessage $ preEscapedText $ msgShow MsgInvalidAccount
                return Nothing
            Nothing -> do
              lift $ setMessage $ preEscapedText $ msgShow MsgNowLogin
              fmap Just $ insert $ User (credsIdent creds) Nothing Nothing True

    authPlugins _ = [ authHashDB
                    , authGoogleEmail
                    ]
                  
    authHttpManager = httpManager

    loginHandler = do
      defaultLayout $ do
        setTitle "Login"
        $(whamletFile "templates/login.hamlet")

instance YesodAuthHashDB App where
    type AuthHashDBId App = UserId

    getPassword uid = runDB $ do
      ma <- get uid
      case ma of
        Nothing -> return Nothing
        Just u -> return $ userPassword u
    setPassword uid encripted = runDB $ update uid [UserPassword =. Just encripted]
    getHashDBCreds account = runDB $ do
        ma <- getBy $ UniqueUser account
        case ma of
            Nothing -> return Nothing
            Just (Entity uid _) -> return $ Just HashDBCreds
                { hashdbCredsId = uid
                , hashdbCredsAuthId = Just uid
                }
    getHashDB = runDB . fmap (fmap userIdent) . get

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
readDoc = readMarkdown defaultParserState . tabFilter (stateTabStop defaultParserState) . T.unpack

wikiWriterOption :: (AppMessage -> Text) -> WriterOptions
wikiWriterOption msgShow =
  defaultWriterOptions{
          writerStandalone = True
        , writerTemplate = "$if(toc)$\n<a id='pandoc-TOC-toggle' href=''></a><div id='pandoc-TOC-Title'>" ++ T.unpack (msgShow MsgTOC) ++ "$toc$</div>\n$endif$\n$body$"
        , writerTableOfContents = False
        , writerNumberSections = False
        , writerIdentifierPrefix = "pandoc-"
        }
sidePaneWriterOption :: WriterOptions
sidePaneWriterOption = 
  defaultWriterOptions{
          writerStandalone = True
        , writerTemplate = "$body$"
        , writerTableOfContents = False
        , writerNumberSections = False
        , writerIdentifierPrefix = "sidepane-"
        }

writeHtmlStr ::  WriterOptions -> (Route App -> [(Text, Text)] -> Text) -> Map.Map Text Wiki -> Pandoc -> String
writeHtmlStr opt render pages = 
  writeHtmlString opt . transformDoc render pages

transformDoc :: (Route App -> [(Text, Text)] -> Text) -> Map.Map Text Wiki -> Pandoc -> Pandoc
transformDoc render pages = bottomUp codeHighlighting . bottomUp (wikiLink render pages)

-- Wiki Link Sign of WikiName is written as [WikiName]().
wikiLink :: (Route App -> [(Text, Text)] -> Text) -> Map.Map Text Wiki -> Inline -> Inline
wikiLink render pages (Link ls ("", "")) = 
  case Map.lookup path' pages of
    Just _  -> 
      Link [Str p'] (render' (WikiR $ fromPath path') [("mode", "v")], p')
    Nothing -> 
      Emph [Str p', Link [Str "?"] (render' NewR [("path", path'), ("mode", "v")], p')]
  where
    p' = inlinesToString ls
    path' = T.pack p'
    render' = (T.unpack .) . render
wikiLink _ _ x = x

codeHighlighting :: Block -> Block
codeHighlighting b@(CodeBlock attr src) = 
  case PH.highlight PH.formatHtmlBlock attr src of
    Just r -> RawBlock "html" $ renderHtml r
    _ -> b
  where
codeHighlighting x = x

findRight :: (a -> Either err v) -> [a] -> Maybe v
findRight _ []     = Nothing
findRight p (a:as) = case p a of
  Left  _ -> findRight p as
  Right x -> Just x
      
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
