{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Root where

import Foundation
import Yesod.AtomFeed
import Yesod.Sitemap
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
      Nothing -> lift $ uncurry (redirectParams RedirectTemporary) topNew
      Just _ -> lift $ uncurry (redirectParams RedirectTemporary) topView

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
    go (_, p) = SitemapUrl (WikiR $ fromWiki p) (wikiUpdated p) Daily 0.9

getFeedR :: Handler RepAtom
getFeedR = runDB $ do
  msgShow <- lift $ getMessageRender
  tops <- selectList  [WikiTouched !=. Nothing] [Desc WikiTouched, LimitTo 10]
  entries <- markdownsToWikiHtmls (noToc msgShow) $ map (wikiContent . snd) tops
  let uday = (wikiUpdated . snd . head) tops
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
    go ((_, w), e) = FeedEntry
      { feedEntryLink = WikiR $ fromWiki w
      , feedEntryUpdated = wikiUpdated w
      , feedEntryTitle = wikiPath w
      , feedEntryContent = e
      }
    noToc r = (wikiWriterOption r)
      { writerTableOfContents = False
      }

getRecentChangesR :: Handler RepJson
getRecentChangesR = do 
  render <- getUrlRender
  entries <- runDB $ selectList [WikiTouched !=. Nothing] [Desc WikiTouched, LimitTo Settings.numOfRecentChanges]
  cacheSeconds 10 -- FIXME
  now <- liftIO getCurrentTime
  jsonToRepJson $ jsonMap [("entries", jsonList $ map (go now render) entries)]
  where
    go now r (_, w) = 
      jsonMap [ ("title", jsonScalar $ T.unpack (wikiPath w))
              , ("uri", jsonScalar $ T.unpack $ r $ WikiR $ fromPath (wikiPath w))
              , ("uday", jsonScalar $ show (wikiUpdated w))
              , ("new", jsonScalar $ show $ ((utctDay now) `diffDays` (utctDay $ wikiUpdated w)) <= Settings.newDays)
              ]

getAuthStatusR :: Handler RepJson
getAuthStatusR = do
  mu <- maybeAuth
  case mu of
    Nothing -> jsonToRepJson $ jsonMap [("status", jsonScalar "401")]
    Just _  -> jsonToRepJson $ jsonMap [("status", jsonScalar "200")]

getAuthToGoR :: Handler ()
getAuthToGoR = do
  go <- lookupGetParam "go"
  case go of
    Nothing -> uncurry (redirectParams RedirectTemporary) topView
    Just r -> do
      _ <- requireAuth
      redirectText RedirectTemporary r

getSystemBatchR :: Handler RepHtml
getSystemBatchR = do
  (_, self) <- requireAuth
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
      redirect RedirectTemporary SystemBatchR
    upgradeThumbnail :: (FileHeaderId, FileHeader) -> IO (FileHeaderId, Maybe Int, Maybe Int, Bool)
    upgradeThumbnail (fid, fh) = do
      let uid = fileHeaderCreator fh 
          s3dir' = Settings.s3dir </> show uid
          s3fp = s3dir' </> show fid
          thumbDir = Settings.s3ThumbnailDir </> show uid
          thumbfp = thumbDir </> show fid
      bs <- L.readFile s3fp
      et <- mkThumbnail bs
      case et of
        Right t -> do
          createDirectoryIfMissing True thumbDir
          saveFile t thumbfp
          return (fid, Just (fst (orgSZ t)), Just (snd (orgSZ t)), True)
        Left _ -> return (fid, Nothing, Nothing, False)
