{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
module Kestrel
    ( Kestrel (..)
    , KestrelRoute (..)
    , resourcesKestrel
    , Handler
    , Widget
    , maybeAuth
    , requireAuth
    , module Yesod
    , module Settings
    , module Model
    , StaticRoute (..)
    , AuthRoute (..)
      -- 
    , WikiPage(..)
    , topPage
    , topNew
    , topView
    , sidePane
    , sidePaneNew
    , simpleSidePane
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
    , UserCrud
    , userCrud
    , (+++)
    ) where

import Yesod
import Yesod.Helpers.Static
import Yesod.Helpers.AtomFeed
import Yesod.Helpers.Auth
import Kestrel.Helpers.Auth.HashDB
import Yesod.Helpers.Auth.OpenId
import Yesod.Helpers.Auth.Facebook
-- import Yesod.Helpers.Auth.OAuth
import Yesod.Helpers.Crud
import Yesod.Form.Jquery
import System.Directory
import qualified Data.ByteString.Lazy as L
import Database.Persist.GenericSql
import Control.Monad (unless, mplus, mzero, MonadPlus)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Applicative ((<$>),(<*>))
import Text.Jasmine (minifym)
import Text.Pandoc
import Text.Pandoc.Shared
import qualified Text.Highlighting.Kate as Kate
import Text.XHtml.Strict (showHtmlFragment)
import qualified Text.ParserCombinators.Parsec as P
import qualified Data.Map as Map (lookup, fromList, Map)
import Web.Encodings (encodeUrl)
import Web.Encodings.StringLike ()
import Data.List (inits)
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as T

import Model
import StaticFiles
import qualified Settings

(+++) :: Text -> Text -> Text
(+++) = T.append

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data Kestrel = Kestrel
    { getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Settings.ConnectionPool -- ^ Database connection pool.
    , isHTTPS :: Bool
    }

-- | A useful synonym; most of the handler functions in your application
-- will need to be of this type.
type Handler = GHandler Kestrel Kestrel

-- | A useful synonym; most of the widgets functions in your application
-- will need to be of this type.
type Widget = GWidget Kestrel Kestrel

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://docs.yesodweb.com/book/web-routes-quasi/
--
-- This function does three things:
--
-- * Creates the route datatype KestrelRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route Kestrel = KestrelRoute
-- * Creates the value resourcesKestrel which contains information on the
--   resources declared below. This is used in Controller.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- Kestrel. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the KestrelRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "Kestrel" [$parseRoutes|
/static StaticR Static getStatic
/auth   AuthR   Auth   getAuth
/auth-status AuthStatusR GET
/auth-go AuthToGoR GET

/profile/#UserId ProfileR GET POST PUT

/favicon.ico FaviconR GET
/robots.txt RobotsR GET
/sitemap.xml SitemapR GET
/feed FeedR GET
/recent-changes.json RecentChangesR GET

/ RootR GET

/admin AdminR UserCrud userCrud

/wiki/*WikiPage WikiR GET POST PUT DELETE
/new NewR GET POST
/histories/*WikiPage HistoriesR GET
/history/#Version/*WikiPage HistoryR GET POST PUT
/either/*WikiPage EitherWikiNewR GET

/wikilist WikiListR GET

/s3/upload UploadR GET POST PUT
/s3/user/#UserId/file/#FileHeaderId FileR GET POST DELETE
/s3/user/#UserId/list.json FileListR GET
|]

newtype WikiPage = WikiPage { unWikiPage :: [Text] } deriving (Eq, Show, Read)
instance MultiPiece WikiPage where
  toMultiPiece = unWikiPage
  fromMultiPiece = Just . WikiPage
  
topPage :: WikiPage
topPage = WikiPage [Settings.topTitle]
topView :: (KestrelRoute, [(Text, Text)])
topView = (WikiR topPage, [("mode","v")])
topNew :: (KestrelRoute, [(Text, Text)])
topNew = (NewR, [("path", encodeUrl Settings.topTitle)])

sidePane :: WikiPage
sidePane = WikiPage [Settings.sidePaneTitle]
sidePaneView :: KestrelRoute
sidePaneView = WikiR sidePane
sidePaneNew :: (KestrelRoute, [(Text, Text)])
sidePaneNew = (NewR, [("path", encodeUrl Settings.sidePaneTitle), ("mode", "e")])

simpleSidePane :: (KestrelRoute, [(Text, Text)])
simpleSidePane = (WikiR sidePane, [("mode", "s")])

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
instance Yesod Kestrel where
    approot app = (if isHTTPS app then "https://" else "http://") +++ Settings.approot +++ Settings.rootbase
    
    defaultLayout widget = do
        y <- getYesod
        mu <- maybeAuth
        mmsg <- getMessage
        r2m <- getRouteToMaster
        cr <- getCurrentRoute
        let mgaUA = Settings.googleAnalyticsUA
            maTUser = Settings.addThisUser
            googleInurl = dropSchema $ approot y
            ga = $(Settings.hamletFile "ga")
            header = $(Settings.hamletFile "header")
            footer = $(Settings.hamletFile "footer")
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
          addCassius $(Settings.cassiusFile "default-layout")
          addJulius $(Settings.juliusFile "default-layout")
          atomLink FeedR $ T.unpack Settings.topTitle
        hamletToRepHtml $(Settings.hamletFile "default-layout")
        
    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticroot setting in Settings.hs
    urlRenderOverride a (StaticR s) =
        Just $ uncurry (joinPath a $ approot a +++ Settings.staticroot) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext' _ content = do
        let fn = base64md5 content ++ '.' : T.unpack ext'
        let content' =
                if ext' == "js"
                    then case minifym content of
                            Left _ -> content
                            Right y -> y
                    else content
        let statictmp = Settings.staticdir ++ "/tmp/"
        liftIO $ createDirectoryIfMissing True statictmp
        let fn' = statictmp ++ fn
        exists <- liftIO $ doesFileExist fn'
        unless exists $ liftIO $ L.writeFile fn' content'
        return $ Just $ Right (StaticR $ StaticRoute ["tmp", T.pack fn] [], [])

-- How to run database actions.
instance YesodPersist Kestrel where
    type YesodDB Kestrel = SqlPersist
    runDB db = liftIOHandler
               $ fmap connPool getYesod >>= Settings.runConnectionPool db
    
instance YesodJquery Kestrel where
  urlJqueryJs _ = Left $ StaticR js_jquery_1_4_4_min_js
  urlJqueryUiJs _ = Left $ StaticR js_jquery_ui_1_8_9_custom_min_js
  urlJqueryUiCss _ = Left $ StaticR css_jquery_ui_1_8_9_custom_css
    
type UserCrud = Crud Kestrel User

instance ToForm User Kestrel where
  toForm mu = fieldsToTable $ User
              <$> stringField "ident" (fmap userIdent mu)
              <*> maybePasswordField "password" Nothing
              <*> maybeStringField "nickname" (fmap userNickname mu)
              <*> boolField "active" (fmap userActive mu)

userCrud :: Kestrel -> Crud Kestrel User
userCrud = const Crud
           { crudSelect = do
                _ <- requireAuth
                runDB $ selectList [] [] 0 0
           , crudReplace = \k a -> do
                _ <- requireAuth
                runDB $ do
                  case userPassword a of
                    Nothing -> do
                      Just a' <- get k
                      replace k $ a {userPassword=userPassword a', userActive=userActive a}
                    Just rp -> do
                      replace k $ a {userPassword=Just $ encrypt rp, userActive=userActive a}
           , crudInsert = \a -> do
                _ <- requireAuth
                runDB $ do
                  insert $ a {userPassword=(fmap encrypt $ userPassword a)}
           , crudGet = \k -> do
                _ <- requireAuth
                runDB $ get k
           , crudDelete = \k -> do
                _ <- requireAuth
                runDB $ delete k
           }

instance YesodAuth Kestrel where
    type AuthId Kestrel = UserId

    -- Where to send a user after successful login
    loginDest _ = RootR
    -- Where to send a user after logout
    logoutDest _ = RootR

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (uid, u) -> 
              if userActive u 
              then do
                lift $ setMessage "ログイン中."
                return $ Just uid 
              else do
                lift $ setMessage "あなたのアカウントは無効です."
                return Nothing
            Nothing -> do
              lift $ setMessage "ログイン中."
              fmap Just $ insert $ User (credsIdent creds) Nothing Nothing True

    authPlugins = [ authHashDB
                  , authOpenId
                  , authFacebook Settings.facebookApplicationId 
                                 Settings.facebookApplicationSecret 
                                 []
                  ]

    loginHandler = do
      defaultLayout $ do
        setTitle "Login"
        addCassius $(Settings.cassiusFile "login")
        addHamlet $(Settings.hamletFile "login")

instance YesodAuthHashDB Kestrel where
    type AuthHashDBId Kestrel = UserId

    getPassword uid = runDB $ do
      ma <- get uid
      case ma of
        Nothing -> return Nothing
        Just u -> return $ userPassword u
    setPassword uid encripted = runDB $ update uid [UserPassword $ Just encripted]
    getHashDBCreds account = runDB $ do
        ma <- getBy $ UniqueUser account
        case ma of
            Nothing -> return Nothing
            Just (uid, _) -> return $ Just HashDBCreds
                { hashdbCredsId = uid
                , hashdbCredsAuthId = Just uid
                }
    getHashDB = runDB . fmap (fmap userIdent) . get

{- markdown utility -}
markdownToWikiHtml :: (Route master ~ KestrelRoute,
                       PersistBackend (t (GGHandler sub master m)),
                       Monad m,
                       Control.Monad.Trans.Class.MonadTrans t) =>
                      WriterOptions -> Text -> t (GGHandler sub master m) Html
markdownToWikiHtml opt raw = do
  render <- lift getUrlRenderParams
  pages <- selectList [] [WikiPathAsc, WikiUpdatedDesc] 0 0
  let pandoc = readDoc raw
  let pdict = mkWikiDictionary pages
  return $ preEscapedString $ writeHtmlStr opt render pdict $ pandoc

markdownsToWikiHtmls
  :: (Route master ~ KestrelRoute,
      PersistBackend (t (GGHandler sub master m)),
      Monad m,
      MonadTrans t) =>
     WriterOptions -> [Text] -> t (GGHandler sub master m) [Html]
markdownsToWikiHtmls opt raws = do
  render <- lift getUrlRenderParams
  pages <- selectList [] [WikiPathAsc, WikiUpdatedDesc] 0 0
  let pandocs = map readDoc raws
  let pdict = mkWikiDictionary pages
  return $ map (preEscapedString . writeHtmlStr opt render pdict) pandocs

readDoc :: Text -> Pandoc
readDoc = readMarkdown defaultParserState . tabFilter (stateTabStop defaultParserState) . T.unpack

wikiWriterOption :: WriterOptions
wikiWriterOption = 
  defaultWriterOptions{
          writerStandalone = True
        , writerTemplate = "$if(toc)$\n<a id='pandoc-TOC-toggle' href=''></a><div id='pandoc-TOC-Title'>目次</div>\n$toc$\n$endif$\n$body$"
        , writerTableOfContents = True
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

writeHtmlStr ::  WriterOptions -> (KestrelRoute -> [(Text, Text)] -> Text) -> Map.Map Text Wiki -> Pandoc -> String
writeHtmlStr opt render pages = 
  writeHtmlString opt . transformDoc render pages

transformDoc :: (KestrelRoute -> [(Text, Text)] -> Text) -> Map.Map Text Wiki -> Pandoc -> Pandoc
transformDoc render pages = processWith codeHighlighting . processWith (wikiLink render pages)

-- Wiki Link Sign of WikiName is written as [WikiName]().
wikiLink :: (KestrelRoute -> [(Text, Text)] -> Text) -> Map.Map Text Wiki -> Inline -> Inline
wikiLink render pages (Link ls ("", "")) = 
  case Map.lookup p'' pages of
    Just _  -> 
      Link [Str p'] (render' (WikiR $ fromPath p'') [("mode", "v")], p')
    Nothing -> 
      Emph [Str p', Link [Str "?"] (render' NewR [("path", path'), ("mode", "v")], p')]
  where
    p' = inlinesToString ls
    p'' = T.pack p'
    path' = encodeUrl p''
    render' = (T.unpack .) . render
wikiLink _ _ x = x

codeHighlighting :: Block -> Block
codeHighlighting b@(CodeBlock (_, attr, _) src) =
  case marry ls langs of
    l:_ ->
      case Kate.highlightAs l src of
        Right result -> RawHtml $ showHtmlFragment $ Kate.formatAsXHtml opts l result
        Left  err    -> RawHtml $ "Could not parse code: " ++ err
    _   -> b
  where
    opts = [Kate.OptNumberLines] `mplus` (findRight (P.parse lineNo "") attr)
    -- Language
    toL = map $ map toLower
    (ls, langs) = (toL attr, toL Kate.languages)
    marry xs ys = [x | x <- xs, y <- ys, x == y]
    -- OptNumberFrom Int
    lineNo :: P.Parser Kate.FormatOption
    lineNo = do
      n <- number
      P.eof
      return $ Kate.OptNumberFrom n
      where
        number :: P.Parser Int
        number = do 
          n <- P.many1 P.digit
          return $ read n
codeHighlighting x = x

findRight :: (MonadPlus m) => (a -> Either err v) -> [a] -> m v
findRight _ []     = mzero
findRight p (a:as) = case p a of
  Left  _ -> findRight p as
  Right x -> return x
      
mkWikiDictionary :: [(WikiId, Wiki)] -> Map.Map Text Wiki
mkWikiDictionary = Map.fromList . map (((,).wikiPath.snd) <*> snd)

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
          Code s                  -> s
          Space                   -> " "
          EmDash                  -> "---"
          EnDash                  -> "--"
          Apostrophe              -> "'"
          Ellipses                -> "..."
          LineBreak               -> " "
          Math DisplayMath s      -> "$$" ++ s ++ "$$"
          Math InlineMath s       -> "$" ++ s ++ "$"
          TeX s                   -> s
          HtmlInline _            -> ""
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
