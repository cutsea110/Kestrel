module Kestrel.WikiParser 
       ( markdownToWikiHtml
       , markdownsToWikiHtmls
       , wikiWriterOption
       , WriterOptions(..)
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

markdownToWikiHtml opt raw = do
  render <- lift getUrlRenderParams
  pages <- selectList [] [WikiPathAsc, WikiUpdatedDesc] 0 0
  let pandoc = readDoc raw
  let pdict = mkWikiDictionary pages
  return $ preEscapedString $ writeHtmlStr opt render pdict $ pandoc

markdownsToWikiHtmls opt raws = do
  render <- lift getUrlRenderParams
  pages <- selectList [] [WikiPathAsc, WikiUpdatedDesc] 0 0
  let pandocs = map readDoc raws
  let pdict = mkWikiDictionary pages
  return $ map (preEscapedString . writeHtmlStr opt render pdict) pandocs

readDoc :: String -> Pandoc
readDoc = readMarkdown defaultParserState . tabFilter (stateTabStop defaultParserState)

wikiWriterOption :: WriterOptions
wikiWriterOption = 
  defaultWriterOptions{
          writerStandalone = True
        , writerTemplate = "$if(toc)$\n<a id='pandoc-TOC-toggle' href=''></a><div id='pandoc-TOC-Title'>Table of Contents</div>\n$toc$\n$endif$\n$body$"
        , writerTableOfContents = True
        , writerNumberSections = False
        , writerIdentifierPrefix = "pandoc-"
        }

-- writeHtmlStr :: WriterOptions (KestrelRoute -> String) -> Map.Map String Wiki -> Pandoc -> String
writeHtmlStr opt render pages = 
  writeHtmlString opt . transformDoc render pages

-- transformDoc :: (KestrelRoute -> String) -> Map.Map String Wiki -> Pandoc -> Pandoc
transformDoc render pages = processWith codeHighlighting . processWith (wikiLink render pages)

-- wikiLink :: (KestrelRoute -> String) -> Map.Map String Wiki -> Inline -> Inline
-- Wiki Link Sign of WikiName is written as [WikiName]().
wikiLink render pages (Link ls ("", "")) = 
  case Map.lookup path pages of
    Just _  -> 
      Link [Str path] (render (WikiR $ fromPath path) [("mode", "v")], path)
    Nothing -> 
      Emph [Str path, Link [Str "?"] (render NewR [("path", path'), ("mode", "v")], path)]
  where
    p' = inlinesToString ls
    path = decodeUrl p'
    path' = encodeUrl p'
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
          Code s                  -> s
          Space                   -> " "
          EmDash                  -> "---"
          EnDash                  -> "--"
          Apostrophe              -> "'"
          Ellipses                -> "..."
          LineBreak               -> " "
          Math DisplayMath s      -> "$$" ++ s ++ "$$"
          Math InlineMath s       -> "$" ++ s ++ "$"
          TeX s                   -> s
          HtmlInline _            -> ""
          Link xs _               -> concatMap go xs
          Image xs _              -> concatMap go xs
          Note _                  -> ""
