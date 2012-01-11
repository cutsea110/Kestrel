{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fspec-constr-count=100 #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Foundation
    ( Kestrel (..)
    , KestrelRoute (..)
    , KestrelMessage (..)
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
--    , UserCrud -- FIXME Crud
--    , userCrud -- FIXME Crud
    , (+++)
    ) where

import Prelude
import Yesod hiding (Form, AppConfig (..), withYamlEnvironment)
import Yesod.Static (Static, base64md5, StaticRoute(..))
import Settings.StaticFiles
import Yesod.AtomFeed
import Yesod.Auth
import Kestrel.Helpers.Auth.HashDB
import Yesod.Auth.OpenId
import Yesod.Auth.Facebook
-- import Yesod.Auth.OAuth
-- import Yesod.Crud
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Logger (Logger, logMsg, formatLogText)
#ifdef DEVELOPMENT
import Yesod.Logger (logLazyText)
#endif
import qualified Settings
import qualified Data.ByteString.Lazy as L
import qualified Database.Persist.Base
import Database.Persist.GenericSql
import Settings (widgetFile)
import Model
import Text.Jasmine (minifym)
import Web.ClientSession (getKey)
import Text.Hamlet (ihamletFile)
import Text.Cassius (cassiusFile)
import Text.Julius (juliusFile)
import Yesod.Form.Jquery
import Control.Monad (mplus, mzero, MonadPlus)
import Control.Applicative ((<*>))
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
import Data.Time.Clock (UTCTime(..))
import qualified Data.Text as T
import Text.Blaze (preEscapedText, preEscapedString)
#if DEVELOPMENT
import qualified Data.Text.Lazy.Encoding
#else
import Network.Mail.Mime (sendmail)
#endif

(+++) :: Text -> Text -> Text
(+++) = T.append

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data Kestrel = Kestrel
    { settings :: AppConfig DefaultEnv ()
    , getLogger :: Logger
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.Base.PersistConfigPool Settings.PersistConfig -- ^ Database connection pool.
    }

mkMessage "Kestrel" "messages" "en"

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
--   resources declared below. This is used in Application.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- Kestrel. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the KestrelRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "Kestrel" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm Kestrel Kestrel (FormResult x, Widget)

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
    approot = appRoot . settings

    encryptKey _ = fmap Just $ getKey "config/client_session_key.aes"
    
    defaultLayout widget = do
        y <- getYesod
        mu <- maybeAuth
        mmsg <- getMessage
        r2m <- getRouteToMaster
        cr <- getCurrentRoute
        msgShow <- getMessageRender
        let mgaUA = Settings.googleAnalyticsUA
            maTUser = Settings.addThisUser
            googleInurl = dropSchema $ approot y
            ga = $(ihamletFile "templates/ga.hamlet")
            header = $(ihamletFile "templates/header.hamlet")
            footer = $(ihamletFile "templates/footer.hamlet")
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
          addCassius $(cassiusFile "templates/default-layout.cassius")
          addJulius $(juliusFile "templates/default-layout.julius")
          atomLink FeedR $ T.unpack Settings.topTitle
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
    
    messageLogger y loc level msg =
      formatLogText (getLogger y) loc level msg >>= logMsg (getLogger y)

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent = addStaticContentExternal minifym base64md5 Settings.staticDir (StaticR . flip StaticRoute [])

    -- Enable Javascript async loading
--    yepnopeJs _ = Just $ Right $ StaticR js_modernizr_js

-- How to run database actions.
instance YesodPersist Kestrel where
    type YesodPersistBackend Kestrel = SqlPersist
    runDB f = liftIOHandler
              $ fmap connPool getYesod >>= Database.Persist.Base.runPool (undefined::Settings.PersistConfig) f
    
instance YesodJquery Kestrel where
  urlJqueryJs _ = Left $ StaticR js_jquery_1_4_4_min_js
  urlJqueryUiJs _ = Left $ StaticR js_jquery_ui_1_8_9_custom_min_js
  urlJqueryUiCss _ = Left $ StaticR css_jquery_ui_1_8_9_custom_css

{-- FIXME Crud
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
--}

instance RenderMessage Kestrel FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodAuth Kestrel where
    type AuthId Kestrel = UserId

    -- Where to send a user after successful login
    loginDest _ = RootR
    -- Where to send a user after logout
    logoutDest _ = RootR

    getAuthId creds = do
      msgShow <- getMessageRender
      runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (uid, u) -> 
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

    authPlugins = [ authHashDB
                  , authOpenId
                  , authFacebook Settings.facebookApplicationId 
                                 Settings.facebookApplicationSecret 
                                 []
                  ]

    loginHandler = do
      defaultLayout $ do
        setTitle "Login"
        addCassius $(cassiusFile "templates/login.cassius")
        addWidget $(whamletFile "templates/login.hamlet")

instance YesodAuthHashDB Kestrel where
    type AuthHashDBId Kestrel = UserId

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
            Just (uid, _) -> return $ Just HashDBCreds
                { hashdbCredsId = uid
                , hashdbCredsAuthId = Just uid
                }
    getHashDB = runDB . fmap (fmap userIdent) . get

-- Sends off your mail. Requires sendmail in production!
deliver :: Kestrel -> L.ByteString -> IO ()
#ifdef DEVELOPMENT
deliver y = logLazyText (getLogger y) . Data.Text.Lazy.Encoding.decodeUtf8
#else
deliver _ = sendmail
#endif


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

wikiWriterOption :: (KestrelMessage -> Text) -> WriterOptions
wikiWriterOption msgShow =
  defaultWriterOptions{
          writerStandalone = True
        , writerTemplate = "$if(toc)$\n<a id='pandoc-TOC-toggle' href=''></a><div id='pandoc-TOC-Title'>" ++ T.unpack (msgShow MsgTOC) ++ "$toc$</div>\n$endif$\n$body$"
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
transformDoc render pages = bottomUp codeHighlighting . bottomUp (wikiLink render pages)

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
        Right result -> RawBlock "html" $ showHtmlFragment $ Kate.formatAsXHtml opts l result
        Left  err    -> RawBlock "html" $ "Could not parse code: " ++ err
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
          EmDash                  -> "---"
          EnDash                  -> "--"
          Apostrophe              -> "'"
          Ellipses                -> "..."
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
