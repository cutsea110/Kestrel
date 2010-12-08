{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Root where

import Kestrel
import Kestrel.WikiParser
import Yesod.Helpers.AtomFeed
import Yesod.Helpers.Sitemap

getRootR :: Handler RepHtml
getRootR = do
    let wikiTop = WikiPage []
    defaultLayout $ do
        h2id <- newIdent
        setTitle "kestrel homepage"
        addWidget $(widgetFile "homepage")

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
  top10 <- selectList [] [WikiUpdatedDesc] 10 0
  entries <- markdownsToWikiHtmls noToc $ map (wikiContent . snd) top10
  let uday = (wikiUpdated . snd . head) top10
  lift $ atomFeed AtomFeed
    { atomTitle = topTitle
    , atomLinkSelf = FeedR
    , atomLinkHome = RootR
    , atomUpdated = uday
    , atomEntries = map go $ zip top10 entries
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
