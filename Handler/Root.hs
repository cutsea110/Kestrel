{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Root where

import Kestrel
import Yesod.Helpers.AtomFeed
import Yesod.Helpers.Sitemap

import qualified Settings

getRootR :: Handler RepHtml
getRootR = uncurry (redirectParams RedirectTemporary) topView
{--
getRootR = do
    let wikiTop = topPage
    defaultLayout $ do
        h2id <- newIdent
        setTitle "kestrel homepage"
        addWidget $(widgetFile "homepage")
--}

-- Some default handlers that ship with the Yesod site template. You will
-- very rarely need to modify this.
getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "favicon.ico"

getRobotsR :: Handler RepPlain
getRobotsR = robots SitemapR -- return $ RepPlain $ toContent "User-agent: *"

getSitemapR :: Handler RepXml
getSitemapR = do
  pages <- runDB $ selectList [] [WikiPathAsc] 0 0
  sitemap $ map go pages
  where
    go (_, p) = SitemapUrl (WikiR $ fromWiki p) (wikiUpdated p) Daily 0.9

getFeedR :: Handler RepAtom
getFeedR = runDB $ do
  tops <- selectList [] [WikiUpdatedDesc] 10 0
  entries <- markdownsToWikiHtmls noToc $ map (wikiContent . snd) tops
  let uday = (wikiUpdated . snd . head) tops
  lift $ atomFeed AtomFeed
    { atomTitle = Settings.topTitle
    , atomLinkSelf = FeedR
    , atomLinkHome = RootR
    , atomUpdated = uday
    , atomEntries = map go $ zip tops entries
    }
  where
    go ((_, w), e) = AtomFeedEntry
      { atomEntryLink = WikiR $ fromWiki w
      , atomEntryUpdated = wikiUpdated w
      , atomEntryTitle = wikiPath w
      , atomEntryContent = e
      }
    noToc = wikiWriterOption
      { writerTableOfContents = False
      }

getRecentChangesR :: Handler RepJson
getRecentChangesR = do 
  render <- getUrlRender
  entries <- runDB $ selectList [] [WikiUpdatedDesc] 10 0
  cacheSeconds 10 -- FIXME
  jsonToRepJson $ jsonMap [("entries", jsonList $ map (go render) entries)]
  where
    go r (wid@(WikiId wid'), w) = 
      jsonMap [ ("title", jsonScalar (wikiPath w))
              , ("uri", jsonScalar $ dropPrefix Settings.approot $ r $ WikiR $ fromPath (wikiPath w))
              , ("uday", jsonScalar $ show (wikiUpdated w))
              ]
