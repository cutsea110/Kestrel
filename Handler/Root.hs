{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Root where

import Kestrel
import Control.Monad

import Text.Pandoc
import Text.Pandoc.Shared
import Language.Haskell.HsColour hiding (string)
import Language.Haskell.HsColour.Colourise (defaultColourPrefs)
import qualified Language.Haskell.HsColour.CSS as CSS
import qualified Text.Highlighting.Kate as Kate
import Text.XHtml.Strict (showHtmlFragment)
import Data.Time
import Control.Applicative ((<$>),(<*>))
import qualified Data.Map as Map (lookup, fromList)
import Yesod.Helpers.Sitemap
import Yesod.Helpers.AtomFeed

getRootR :: Handler RepHtml
getRootR = do
    let wikiTop = WikiPage []
    defaultLayout $ do
        h2id <- newIdent
        setTitle "kestrel homepage"
        addWidget $(widgetFile "homepage")

getFeedR :: Handler RepAtom
getFeedR = do
  pages <- runDB $ selectList [] [WikiUpdatedDesc] 0 0
  let uday = (wikiUpdated . snd . head) pages
  atomFeed AtomFeed
    { atomTitle = topTitle
    , atomLinkSelf = FeedR
    , atomLinkHome = RootR
    , atomUpdated = uday
    , atomEntries = map go $ take 10 pages
    }
  where
    go (_, w) = AtomFeedEntry
      { atomEntryLink = WikiR $ fromWiki w
      , atomEntryUpdated = wikiUpdated w
      , atomEntryTitle = wikiPath w
      , atomEntryContent = undefined
      }
