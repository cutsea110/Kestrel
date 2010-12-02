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

getRootR :: Handler RepHtml
getRootR = do
    let wikiTop = WikiPage []
    defaultLayout $ do
        h2id <- newIdent
        setTitle "kestrel homepage"
        addWidget $(widgetFile "homepage")
