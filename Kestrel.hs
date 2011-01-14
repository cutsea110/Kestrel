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
    , topView
    , sidePane
    , sidePaneNew
    , simpleSidePane
    , lastNameOf
    , pathOf
    , fromPath
    , fromWiki
    , ancestory
    , setpassR -- Auth.Account
    , getBy404
      --
    , markdownToWikiHtml
    , markdownsToWikiHtmls
    , wikiWriterOption
    , WriterOptions(..)
    , showDate
    , dropPrefix
      --
    , UserCrud
    , userCrud
    ) where

import Yesod
import Yesod.Helpers.Static
import Yesod.Helpers.AtomFeed
import Yesod.Helpers.Auth
import qualified Kestrel.Helpers.Auth.Account as A (saltPass)
import Kestrel.Helpers.Auth.Account
import Yesod.Helpers.Auth.OpenId
import Yesod.Helpers.Auth.Email
import Yesod.Helpers.Crud
import Yesod.Form.Jquery
import System.Directory
import qualified Data.ByteString.Lazy as L
import Web.Routes.Site (Site (formatPathSegments))
import Database.Persist.GenericSql
import Data.Maybe (isJust)
import Control.Monad (join, unless, mplus, mzero, MonadPlus)
import Control.Applicative ((<$>),(<*>))
import Network.Mail.Mime
import Network.Wai
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import Text.Jasmine (minifym)
import Text.Pandoc
import Text.Pandoc.Shared
import qualified Text.Highlighting.Kate as Kate
import Text.XHtml.Strict (showHtmlFragment)
import Data.Char (toLower)
import qualified Text.ParserCombinators.Parsec as P
import Data.Time
import System.Locale
import qualified Data.Map as Map (lookup, fromList)
import Web.Encodings (encodeUrl, decodeUrl)
import Data.List (intercalate, inits)
import Data.List.Split (splitOn)

import Model
import StaticFiles
import qualified Settings

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data Kestrel = Kestrel
    { getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Settings.ConnectionPool -- ^ Database connection pool.
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

/favicon.ico FaviconR GET
/robots.txt RobotsR GET
/sitemap.xml SitemapR GET
/feed FeedR GET
/recent-changes.json RecentChangesR GET

/ RootR GET

/admin AdminR UserCrud userCrud

/wiki/*WikiPage WikiR GET POST PUT DELETE
/new NewR GET POST
/history/*WikiPage HistoryR GET POST

/s3/upload UploadR GET POST PUT
/s3/user/#UserId/file/#FileHeaderId FileR GET POST
/s3/user/#UserId/list.json FileListR GET
|]

newtype WikiPage = WikiPage { unWikiPage :: [String] } deriving (Eq, Show, Read)
instance MultiPiece WikiPage where
  toMultiPiece = unWikiPage
  fromMultiPiece = Right . WikiPage

topPage :: WikiPage
topPage = WikiPage [Settings.topTitle]
topView :: (KestrelRoute, [(String, String)])
topView = (WikiR topPage, [("mode","v")])

sidePane :: WikiPage
sidePane = WikiPage [Settings.sidePaneTitle]
sidePaneNew :: (KestrelRoute, [(String, String)])
sidePaneNew = (NewR, [("path", encodeUrl Settings.sidePaneTitle), ("mode", "e")])

simpleSidePane :: (KestrelRoute, [(String, String)])
simpleSidePane = (WikiR sidePane, [("mode", "s")])

pathOf :: WikiPage -> String
pathOf = intercalate ":" . unWikiPage

fromPath :: String -> WikiPage
fromPath path = WikiPage $ splitOn ":" path

fromWiki :: Wiki -> WikiPage
fromWiki = fromPath . wikiPath

lastNameOf :: WikiPage -> String
lastNameOf = last . unWikiPage

ancestory :: WikiPage -> [WikiPage]
ancestory = map WikiPage . filter (/=[]) . inits . unWikiPage

getBy404 ukey = do
  mres <- getBy ukey
  case mres of
    Nothing -> lift notFound
    Just res -> return res

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod Kestrel where
    approot _ = Settings.approot
    
    onRequest = do
      req' <- getRequest
      let req = reqWaiRequest req'
      (pp, files) <- liftIO $ reqRequestBody req'
      ses <- getSession
      -- Network.Wai.Request
      liftIO $ do
        putStrLn "----"
        putStrLn $ "Method: " ++ (show . requestMethod) req
        putStrLn $ "httpVer.: " ++ (show . httpVersion) req
        putStrLn $ "Path: " ++ (show . pathInfo) req
        putStrLn $ "query string: " ++ (show . queryString) req
        putStrLn $ "Server Name: " ++ (show . serverName) req
        putStrLn $ "Server Port: " ++ (show . serverPort) req
        putStrLn $ "Request Headers: " ++ (show . requestHeaders) req
        putStrLn $ "Secure: " ++ if isSecure req then "YES" else "NO"
        putStrLn $ "Client Host Infomation: " ++ (show . remoteHost) req
        -- Yesod.Request
        putStrLn $ "Cookies: " ++ (show . reqCookies) req'
        putStrLn $ "Lang: " ++ (show . reqLangs) req'
        putStrLn $ "Nonce: " ++ reqNonce req'
        putStrLn $ "GET: " ++ (show . reqGetParams) req'
        putStrLn $ "POST: " ++ show pp ++ show files
        -- Session
        putStrLn $ "Session: " ++ show ses

    
    defaultLayout widget = do
        y <- getYesod
        render <- getUrlRender
        mu <- maybeAuth
        mmsg <- getMessage
        (spTitle, msp) <- runDB $ do
          msp' <- getBy $ UniqueWiki Settings.sidePaneTitle
          case msp' of
            Nothing -> return (Settings.sidePaneTitle, Nothing)
            Just (_, sp'') -> do
              sp' <- markdownToWikiHtml wikiWriterOption $ wikiContent sp''
              return (Settings.sidePaneTitle, Just sp')
        let header = $(Settings.hamletFile "header")
            footer = $(Settings.hamletFile "footer")
        pc <- widgetToPageContent $ do
          widget
          addScriptEither $ urlJqueryJs y
          addScriptEither $ urlJqueryUiJs y
          addStylesheetEither $ urlJqueryUiCss y
          addScriptEither $ Left $ StaticR js_jquery_upload_1_0_2_js
          addScriptEither $ Left $ StaticR js_jquery_bubbleup_js
          addCassius $(Settings.cassiusFile "default-layout")
          addJulius $(Settings.juliusFile "default-layout")
          atomLink FeedR Settings.topTitle
        hamletToRepHtml $(Settings.hamletFile "default-layout")
        
    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticroot setting in Settings.hs
    urlRenderOverride a (StaticR s) =
        Just $ uncurry (joinPath a Settings.staticroot) $ format s
      where
        format = formatPathSegments ss
        ss :: Site StaticRoute (String -> Maybe (GHandler Static Kestrel ChooseRep))
        ss = getSubSite
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext' _ content = do
        let fn = base64md5 content ++ '.' : ext'
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
        return $ Just $ Right (StaticR $ StaticRoute ["tmp", fn] [], [])

-- How to run database actions.
instance YesodPersist Kestrel where
    type YesodDB Kestrel = SqlPersist
    runDB db = fmap connPool getYesod >>= Settings.runConnectionPool db
    
instance YesodJquery Kestrel where
  urlJqueryJs _ = Left $ StaticR js_jquery_js
  urlJqueryUiJs _ = Left $ StaticR js_jquery_ui_js
  urlJqueryUiCss _ = Left $ StaticR css_jquery_ui_css
    
instance Item User where
  itemTitle = userIdent

type UserCrud = Crud Kestrel User

instance ToForm User Kestrel where
  toForm mu = fieldsToTable $ User
              <$> stringField "ident" (fmap userIdent mu)
              <*> passwordField' "password" (fmap userPassword mu)

userCrud :: Kestrel -> Crud Kestrel User
userCrud = const Crud
           { crudSelect = do
                _ <- requireAuth
                runDB $ selectList [] [] 0 0
           , crudReplace = \k a -> do
                _ <- requireAuth
                runDB $ do
                  case userPassword a of
                    "" -> do
                      Just a' <- get k
                      replace k $ User (userIdent a) (userPassword a')
                    rp -> do
                      salted <- liftIO $ A.saltPass rp
                      replace k $ User (userIdent a) salted
           , crudInsert = \a -> do
                _ <- requireAuth
                runDB $ do
                  salted <- liftIO $ A.saltPass $ userPassword a
                  insert $ User (userIdent a) salted
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
            Just (uid, _) -> return $ Just uid
            Nothing -> do
              fmap Just $ insert $ User (credsIdent creds) ""

    showAuthId _ = showIntegral
    readAuthId _ = readIntegral

    authPlugins = [ authAccount
                  , authOpenId
                  , authEmail ]

instance YesodAuthAccount Kestrel where
    type AuthAccountId Kestrel = UserId

    showAuthAccountId _ = showIntegral
    readAuthAccountId _ = readIntegral

    getPassword uid = runDB $ return . fmap userPassword =<< get uid
    setPassword uid salted = runDB $ update uid [UserPassword salted]
    getAccountCreds account = runDB $ do
        ma <- getBy $ UniqueUser account
        case ma of
            Nothing -> return Nothing
            Just (uid, _) -> return $ Just AccountCreds
                { accountCredsId = uid
                , accountCredsAuthId = Just uid
                }
    getAccount = runDB . fmap (fmap userIdent) . get


instance YesodAuthEmail Kestrel where
    type AuthEmailId Kestrel = EmailId

    showAuthEmailId _ = showIntegral
    readAuthEmailId _ = readIntegral

    addUnverified email verkey =
        runDB $ insert $ Email email Nothing $ Just verkey
    sendVerifyEmail email _ verurl = liftIO $ renderSendMail Mail
        { mailHeaders =
            [ ("From", "noreply")
            , ("To", email)
            , ("Subject", "Verify your email address")
            ]
        , mailParts = [[textPart, htmlPart]]
        }
      where
        textPart = Part
            { partType = "text/plain; charset=utf-8"
            , partEncoding = None
            , partFilename = Nothing
            , partContent = Data.Text.Lazy.Encoding.encodeUtf8
                          $ Data.Text.Lazy.pack $ unlines
                [ "Please confirm your email address by clicking on the link below."
                , ""
                , verurl
                , ""
                , "Thank you"
                ]
            }
        htmlPart = Part
            { partType = "text/html; charset=utf-8"
            , partEncoding = None
            , partFilename = Nothing
            , partContent = renderHtml [$hamlet|
%p Please confirm your email address by clicking on the link below.
%p
    %a!href=$verurl$ $verurl$
%p Thank you
|]
            }
    getVerifyKey = runDB . fmap (join . fmap emailVerkey) . get
    setVerifyKey eid key = runDB $ update eid [EmailVerkey $ Just key]
    verifyAccount eid = runDB $ do
        me <- get eid
        case me of
            Nothing -> return Nothing
            Just e -> do
                let email = emailEmail e
                case emailUser e of
                    Just uid -> return $ Just uid
                    Nothing -> do
                        uid <- insert $ User email ""
                        update eid [EmailUser $ Just uid, EmailVerkey Nothing]
                        return $ Just uid
    getPassword uid = runDB $ return . fmap userPassword =<< get uid
    setPassword uid salted = runDB $ update uid [UserPassword salted]
    getEmailCreds email = runDB $ do
        me <- getBy $ UniqueEmail email
        case me of
            Nothing -> return Nothing
            Just (eid, e) -> return $ Just EmailCreds
                { emailCredsId = eid
                , emailCredsAuthId = emailUser e
                , emailCredsStatus = isJust $ emailUser e
                , emailCredsVerkey = emailVerkey e
                }
    getEmail = runDB . fmap (fmap emailEmail) . get


{- markdown utility -}
markdownToWikiHtml opt raw = do
  render <- lift getUrlRenderParams
  pages <- selectList [] [WikiPathAsc, WikiUpdatedDesc] 0 0
  let pandoc = readDoc raw
  let pdict = mkWikiDictionary pages
  return $ preEscapedString $ writeHtmlStr opt render pdict $ pandoc

markdownsToWikiHtmls opt raws = do
  render <- lift getUrlRenderParams
  pages <- selectList [] [WikiPathAsc, WikiUpdatedDesc] 0 0
  let pandocs = map readDoc raws
  let pdict = mkWikiDictionary pages
  return $ map (preEscapedString . writeHtmlStr opt render pdict) pandocs

readDoc :: String -> Pandoc
readDoc = readMarkdown defaultParserState . tabFilter (stateTabStop defaultParserState)

wikiWriterOption :: WriterOptions
wikiWriterOption = 
  defaultWriterOptions{
          writerStandalone = True
        , writerTemplate = "$if(toc)$\n<a id='pandoc-TOC-toggle' href=''></a><div id='pandoc-TOC-Title'>Table of Contents</div>\n$toc$\n$endif$\n$body$"
        , writerTableOfContents = True
        , writerNumberSections = False
        , writerIdentifierPrefix = "pandoc-"
        }

-- writeHtmlStr :: WriterOptions (KestrelRoute -> String) -> Map.Map String Wiki -> Pandoc -> String
writeHtmlStr opt render pages = 
  writeHtmlString opt . transformDoc render pages

-- transformDoc :: (KestrelRoute -> String) -> Map.Map String Wiki -> Pandoc -> Pandoc
transformDoc render pages = processWith codeHighlighting . processWith (wikiLink render pages)

-- wikiLink :: (KestrelRoute -> String) -> Map.Map String Wiki -> Inline -> Inline
-- Wiki Link Sign of WikiName is written as [WikiName]().
wikiLink render pages (Link ls ("", "")) = 
  case Map.lookup path pages of
    Just _  -> 
      Link [Str path] (render (WikiR $ fromPath path) [("mode", "v")], path)
    Nothing -> 
      Emph [Str path, Link [Str "?"] (render NewR [("path", path'), ("mode", "v")], path)]
  where
    p' = inlinesToString ls
    path = decodeUrl p'
    path' = encodeUrl p'
wikiLink _ _ x = x

codeHighlighting :: Block -> Block
codeHighlighting b@(CodeBlock (_, attr, _) src) =
  case marry xs langs of
    l:_ ->
      case Kate.highlightAs l src of
        Right result -> RawHtml $ showHtmlFragment $ Kate.formatAsXHtml opts l result
        Left  err    -> RawHtml $ "Could not parse code: " ++ err
    _   -> b
  where
    opts = [Kate.OptNumberLines] `mplus` (findRight (P.parse lineNo "") attr)
    -- Language
    toL = map $ map toLower
    (xs, langs) = (toL attr, toL Kate.languages)
    marry xs ys = [x | x <- xs, y <- ys, x == y]
    -- OptNumberFrom Int
    lineNo :: P.Parser Kate.FormatOption
    lineNo = do
      pref <- P.string "lineFrom"
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
      
-- mkWikiDictionary :: [(Key Wiki, Wiki)] -> Map.Map String Wiki
mkWikiDictionary = Map.fromList . map (((,).wikiPath.snd) <*> snd)

showDate :: UTCTime -> String
showDate = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

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
dropPrefix :: (Eq a) => [a] -> [a] -> [a]
dropPrefix xs ys = dp' ys xs ys
  where
    dp' :: (Eq a) => [a] -> [a] -> [a] -> [a]
    dp' _  []     ys'     = ys'
    dp' os _      []      = os
    dp' os (x:xs') (y:ys') | x==y = dp' os xs' ys'
                           | otherwise = os
