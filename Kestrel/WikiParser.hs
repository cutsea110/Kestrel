module Kestrel.WikiParser 
       ( markdownToWikiHtml
       , showDate
       ) where

import Kestrel
import Model
import Control.Monad

import Text.Pandoc
import Text.Pandoc.Shared
import qualified Text.Highlighting.Kate as Kate
import Text.XHtml.Strict (showHtmlFragment)
import Data.Char (toLower)
import qualified Text.ParserCombinators.Parsec as P
import Data.Time
import System.Locale
import Control.Applicative ((<$>),(<*>))
import qualified Data.Map as Map (lookup, fromList)
import Web.Encodings (encodeUrl, decodeUrl)

markdownToWikiHtml raw = do
  render <- lift getUrlRenderParams
  pages <- selectList [] [WikiPathAsc, WikiUpdatedDesc] 0 0
  let pandoc = readDoc raw
  let pdict = mkWikiDictionary pages
  return $ preEscapedString $ writeHtmlStr render pdict $ pandoc

readDoc :: String -> Pandoc
readDoc = readMarkdown defaultParserState . tabFilter (stateTabStop defaultParserState)

-- writeHtmlStr :: (KestrelRoute -> String) -> Map.Map String Wiki -> Pandoc -> String
writeHtmlStr render pages = 
  writeHtmlString opt . transformDoc render pages
    where
      opt = defaultWriterOptions{
          writerStandalone = True
        , writerTemplate = "$if(toc)$\n<div id='pandoc-TOC-Title'>Table of Contents</div>\n$toc$\n$endif$\n$body$"
        , writerTableOfContents = True
        , writerNumberSections = True
        , writerIdentifierPrefix = "pandoc-"
        }

-- transformDoc :: (KestrelRoute -> String) -> Map.Map String Wiki -> Pandoc -> Pandoc
transformDoc render pages = processWith codeHighlighting . processWith (wikiLink render pages)

-- wikiLink :: (KestrelRoute -> String) -> Map.Map String Wiki -> Inline -> Inline
-- Wiki Link Sign of WikiName is written as [](WikiName).
wikiLink render pages (Link [] (s, "")) = 
  case Map.lookup path pages of
    Just _  -> 
      Link [Str path] (render (WikiR $ fromPath path) [("mode", "v")], path)
    Nothing -> 
      Emph [Str path, Link [Str "?"] (render NewR [("path", s), ("mode", "v")], path)]
  where
    path = decodeUrl s
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
