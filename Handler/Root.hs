{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Handler.Root where

import Foundation
import Yesod
import Yesod.AtomFeed
import Yesod.Sitemap
import Data.Maybe (fromJust)
import Data.Time
import qualified Data.Text as T
import Control.Monad (forM_)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)
import qualified Data.ByteString.Lazy as L
import Graphics.Thumbnail

import qualified Settings

getRootR :: Handler RepHtml
getRootR = do
  runDB $ do
    let path = pathOf topPage
    top <- getBy $ UniqueWiki path
    case top of
      Nothing -> lift $ redirect topNew
      Just _ -> lift $ redirect topView

-- Some default handlers that ship with the Yesod site template. You will
-- very rarely need to modify this.
getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "config/favicon.ico"

getRobotsR :: Handler RepPlain
getRobotsR = robots SitemapR -- return $ RepPlain $ toContent "User-agent: *"

getSitemapR :: Handler RepXml
getSitemapR = do
  pages <- runDB $ selectList [] [Asc WikiPath]
  sitemap $ map go pages
  where
    go (Entity _ p) = SitemapUrl (WikiR $ fromWiki p) (wikiUpdated p) Daily 0.9

getFeedR :: Handler RepAtom
getFeedR = runDB $ do
  msgShow <- lift $ getMessageRender
  tops <- selectList  [WikiTouched !=. Nothing] [Desc WikiTouched, LimitTo 10]
  entries <- markdownsToWikiHtmls (noToc msgShow) $ map (wikiContent . entityVal) tops
  let uday = (wikiUpdated . entityVal . head) tops
  lift $ atomFeed Feed
    { feedTitle = Settings.topTitle
    , feedDescription = ""
    , feedLanguage = ""
    , feedLinkSelf = FeedR
    , feedLinkHome = RootR
    , feedUpdated = uday
    , feedEntries = map go $ zip tops entries
    }
  where
    go ((Entity _ w), e) = FeedEntry
      { feedEntryLink = WikiR $ fromWiki w
      , feedEntryUpdated = wikiUpdated w
      , feedEntryTitle = wikiPath w
      , feedEntryContent = e
      }
    noToc r = (wikiWriterOption r)
      { writerTableOfContents = False
      }

getRecentChangesR :: Handler RepHtmlJson
getRecentChangesR = do 
  render <- getUrlRender
  entries <- runDB $ selectList [WikiTouched !=. Nothing] [Desc WikiTouched, LimitTo Settings.numOfRecentChanges]
  now <- liftIO getCurrentTime
  let widget = $(widgetFile "recentChanges")
      json = object ["entries" .= array (map (go now render) entries)]
  defaultLayoutJson widget json
  where
    (sec,minute,hour,day,month,year) = (1,60*sec,60*minute,24*hour,30*day,365*day)
    fromSec d s = ceiling $ d / s
    go now r (Entity _ w) = 
      object [ "title" .= wikiPath w
             , "uri" .= r (WikiR (fromPath (wikiPath w)))
             , "uday" .= wikiUpdated w
             , "toucheddate" .= fromJust (wikiTouched w)
             , "new" .= (((utctDay now) `diffDays` utctDay (fromJust (wikiTouched w))) <= Settings.newDays)
             ]

getAuthStatusR :: Handler RepJson
getAuthStatusR = do
  mu <- maybeAuth
  case mu of
    Nothing -> jsonToRepJson $ object ["status" .= ("401" :: T.Text)]
    Just _  -> jsonToRepJson $ object ["status" .= ("200" :: T.Text)]

getAuthToGoR :: Handler ()
getAuthToGoR = do
  go <- lookupGetParam "go"
  case go of
    Nothing -> redirect topView
    Just r -> do
      _ <- requireAuth
      redirect r

getSystemBatchR :: Handler RepHtml
getSystemBatchR = do
  (Entity _ self) <- requireAuth
  defaultLayout $ do
    setTitle "システムバッチ"
    addWidget $(widgetFile "systembatch")

postSystemBatchR :: Handler RepHtml
postSystemBatchR = do
  _ <- requireAuth
  method <- lookupPostParam "_method"
  case method of
    Just "thumbnail_update" -> thumbnailUpdate
    _ -> invalidArgs ["The possible value of '_method' is thumbnail_update."]
  where
    thumbnailUpdate :: Handler RepHtml
    thumbnailUpdate = do
      runDB $ do
        files <- selectList [] []
        files' <- liftIO $ mapM upgradeThumbnail files
        forM_ files' $ \(fid, w, h, imgp) -> do
          update fid [ FileHeaderWidth =. w 
                     , FileHeaderHeight =. h
                     , FileHeaderThumbnail =. imgp ]
      redirect SystemBatchR
    upgradeThumbnail :: Entity FileHeader -> IO (FileHeaderId, Maybe Int, Maybe Int, Bool)
    upgradeThumbnail (Entity fid fh) = do
      let uid = fileHeaderCreator fh 
          s3dir' = Settings.s3dir </> T.unpack (toPathPiece uid)
          s3fp = s3dir' </> T.unpack (toPathPiece fid)
          thumbDir = Settings.s3ThumbnailDir </> T.unpack (toPathPiece uid)
          thumbfp = thumbDir </> T.unpack (toPathPiece fid)
      bs <- L.readFile s3fp
      et <- mkThumbnail bs
      case et of
        Right t -> do
          createDirectoryIfMissing True thumbDir
          saveFile t thumbfp
          return (fid, Just (fst (orgSZ t)), Just (snd (orgSZ t)), True)
        Left _ -> return (fid, Nothing, Nothing, False)
