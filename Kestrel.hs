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
    , lastNameOf
    , pathOf
    , fromPath
    , fromWiki
    , ancestory
    , topTitle
    , setpassR -- Auth.Account
      --
    , UserCrud
    , userCrud
    ) where

import Yesod
import Yesod.Helpers.Static
import Yesod.Helpers.AtomFeed
import Yesod.Helpers.Auth
import Yesod.Helpers.Auth.OpenId
import Yesod.Helpers.Crud
import qualified Settings
import Settings (hamletFile, cassiusFile, juliusFile, widgetFile)
import System.Directory
import qualified Data.ByteString.Lazy as L
import Web.Routes.Site (Site (formatPathSegments))
import Database.Persist.GenericSql
import Data.Maybe (isJust)
import Control.Monad (join, unless)
import Control.Applicative ((<$>),(<*>))
import Network.Mail.Mime
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import Text.Jasmine (minifym)

import Kestrel.Helpers.Auth.Account
import Data.List (intercalate, inits)
import Data.List.Split (splitOn)

import Model

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
/feed FeedR GET

/ RootR GET

/admin AdminR UserCrud userCrud

/wiki/*WikiPage     WikiR GET POST PUT DELETE
/new NewR GET POST
|]

newtype WikiPage = WikiPage { unWikiPage :: [String] } deriving (Eq, Show, Read)
instance MultiPiece WikiPage where
  toMultiPiece = unWikiPage
  fromMultiPiece = Right . WikiPage

topTitle :: String
topTitle = "Kestrel = WIKI"
topPage :: WikiPage
topPage = WikiPage []
topView :: (KestrelRoute, [(String, String)])
topView = (WikiR topPage, [("mode","v")])

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


-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod Kestrel where
    approot _ = Settings.approot
    
    defaultLayout widget = do
        mu <- maybeAuth
        mmsg <- getMessage
        pc <- widgetToPageContent $ do
          widget
          addCassius $(Settings.cassiusFile "default-layout")
          atomLink FeedR topTitle
        let header = $(hamletFile "header")
        let footer = $(hamletFile "footer")
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
    
instance Item User where
  itemTitle = userIdent

type UserCrud = Crud Kestrel User

instance ToForm User Kestrel where
  toForm mu = fieldsToTable $ User
              <$> stringField "ident" (fmap userIdent mu)
              <*> maybePasswordField' "password" (fmap userPassword mu)

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
                      replace k $ User (userIdent a) (userPassword a')
                    Just rp -> do
                      salted <- liftIO $ saltPass rp
                      replace k $ User (userIdent a) (Just salted)
           , crudInsert = \a -> do
                _ <- requireAuth
                runDB $ do
                  case userPassword a of
                    Nothing -> insert a -- FIXME
                    Just rp -> do
                      salted <- liftIO $ saltPass rp
                      insert $ User (userIdent a) (Just salted)
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
              fmap Just $ insert $ User (credsIdent creds) Nothing

    showAuthId _ = showIntegral
    readAuthId _ = readIntegral

    authPlugins = [ authAccount, authOpenId ]

instance YesodAuthAccount Kestrel where
    type AuthAccountId Kestrel = UserId

    showAuthAccountId _ = showIntegral
    readAuthAccountId _ = readIntegral

    getPassword uid = runDB $ return . join . fmap userPassword =<< get uid
    setPassword uid salted = runDB $ update uid [UserPassword $ Just salted]
    getAccountCreds account = runDB $ do
        ma <- getBy $ UniqueUser account
        case ma of
            Nothing -> return Nothing
            Just (uid, _) -> return $ Just AccountCreds
                { accountCredsId = uid
                , accountCredsAuthId = Just uid
                }
    getAccount = runDB . fmap (fmap userIdent) . get
