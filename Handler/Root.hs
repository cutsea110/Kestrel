{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Root where

import Kestrel
import Kestrel.WikiParser
import Yesod.Helpers.AtomFeed

getRootR :: Handler RepHtml
getRootR = do
    let wikiTop = WikiPage []
    defaultLayout $ do
        h2id <- newIdent
        setTitle "kestrel homepage"
        addWidget $(widgetFile "homepage")

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
