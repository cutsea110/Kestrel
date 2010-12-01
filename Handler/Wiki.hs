{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Wiki where

import Kestrel
import Control.Monad

import Text.Pandoc
import Text.Pandoc.Shared
import qualified Text.Highlighting.Kate as Kate
import Text.XHtml.Strict (showHtmlFragment)
import Data.Char (toLower)
import qualified Text.ParserCombinators.Parsec as P
import Data.Time
import Control.Applicative ((<$>),(<*>))
import qualified Data.Map as Map (lookup, fromList)
import Web.Encodings (encodeUrl, decodeUrl)

import StaticFiles

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

getWikiR :: WikiPage -> Handler RepHtml
getWikiR wp = do
  mode <- lookupGetParam "mode"
  case mode of
    Just "v" {-  view  page  -} -> viewWiki
    Just "e" {-  edit  page  -} -> editWiki
    Just _   {- default mode -} -> viewWiki  -- FIXME
    Nothing  {- default mode -} -> viewWiki  -- FIXME
  where
    -- Utility
    getwiki :: Handler (Maybe (String, String, Html, Int, Bool))
    getwiki = do
      render <- getUrlRenderParams
      let path = pathOf wp
      (page, pages) <- runDB $ do
        page'  <- getBy $ UniqueWiki path
        pages' <- selectList [] [WikiPathAsc, WikiUpdatedDesc] 0 0
        return (page', mkWikiDictionary pages')
      case page of
        Nothing -> return Nothing
        Just p  -> do
          let (raw, ver) = ((,).wikiContent) <*> wikiVersion $ snd p
          let pandoc = readDoc raw
          let content = preEscapedString $ writeHtmlStr render pages $ pandoc
          let isTop = wp == topPage
          return $ Just (path, raw, content, ver, isTop)
    
    -- Pages    
    viewWiki :: Handler RepHtml
    viewWiki = do
      mu <- maybeAuth
      wiki <- getwiki
      case wiki of
        Nothing -> notFound
        Just (path, raw, content, ver, isTop) -> do
          let editMe = (WikiR wp, [("mode", "e")])
          defaultLayout $ do
            setTitle $ string $ if isTop then topTitle else path
            addCassius $(cassiusFile "wiki")
            addStylesheet $ StaticR hk_kate_css
            addWidget $(widgetFile "viewWiki")

    editWiki :: Handler RepHtml
    editWiki = do
      (uid, _) <- requireAuth
      wiki <- getwiki
      case wiki of
        Nothing -> notFound
        Just (path, raw, content, ver, isTop) -> do
          defaultLayout $ do
            setTitle $ string $ if isTop then topTitle else path
            addCassius $(cassiusFile "wiki")
            addStylesheet $ StaticR hk_kate_css
            addWidget $(widgetFile "editWiki")

postWikiR :: WikiPage -> Handler RepHtml
postWikiR wp = do
  (uid, _) <- requireAuth
  submits@(preview, commit) <-
    uncurry (liftM2 (,)) (lookupPostParam "preview", lookupPostParam "commit")
  case submits of
    (Nothing, Nothing) -> invalidArgs ["'preview' or 'commit' parameter is required"]
    (Just _,  Just _)  -> invalidArgs ["'preview' and 'commit' parameters are alternative"]
    (Just _,  Nothing) -> previewWiki
    (Nothing, Just _)  -> updateWiki
  where
    previewWiki :: Handler RepHtml
    previewWiki = do
      render <- getUrlRenderParams
      let path = pathOf wp
      pages <- runDB $ do
        pages' <- selectList [] [WikiPathAsc, WikiUpdatedDesc] 0 0
        return $ mkWikiDictionary pages'
      (raw, ver) <- runFormPost' $ (,)
                    <$> stringInput "content"
                    <*> intInput "version"
      let pandoc = readDoc raw
      let content = preEscapedString $ writeHtmlStr render pages $ pandoc
      let isTop = wp == topPage
      defaultLayout $ do
        setTitle $ string $ if isTop then topTitle else path
        addCassius $(cassiusFile "wiki")
        addStylesheet $ StaticR hk_kate_css
        addWidget $(widgetFile "previewWiki")
    
    updateWiki :: Handler RepHtml
    updateWiki = do
      render <- getUrlRenderParams
      let path = pathOf wp
      now <- liftIO getCurrentTime
      (raw, ver) <- runFormPost' $ (,)
                    <$> stringInput "content"
                    <*> intInput "version"
      id <- runDB $ do
        wiki <- getBy $ UniqueWiki path
        case wiki of
          Nothing -> return Nothing
          Just (pid, page) -> do
            if wikiVersion page == ver
              then do
              insert WikiHistory{ 
                  wikiHistoryWiki=pid 
                , wikiHistoryPath=(wikiPath page) 
                , wikiHistoryContent=(wikiContent page) 
                , wikiHistoryUpdated=(wikiUpdated page) 
                , wikiHistoryVersion=(wikiVersion page)
                , wikiHistoryEditor=(wikiEditor page)
                , wikiHistoryDeleted=False
                }
              update pid [WikiContent raw, WikiUpdated now, WikiVersionAdd 1]
              return $ Just pid
              else do
              lift $ setMessage $ string "conflict occured. can't save your modify."
              return $ Just pid
      case id of
        Nothing -> notFound -- FIXME invalidArgs?
        Just _ -> redirectParams RedirectSeeOther (WikiR wp) [("mode", "v")]



getNewR :: Handler RepHtml
getNewR = do
  mode <- lookupGetParam "mode"
  case mode of
    Just "v" {-  view  page  -} -> viewNew
    Just "e" {-  edit  page  -} -> editNew
    Just _   {- default mode -} -> viewNew  -- FIXME
    Nothing  {- default mode -} -> viewNew  -- FIXME
  where
    viewNew :: Handler RepHtml
    viewNew = do
      mu <- maybeAuth
      path'' <- lookupGetParam "path"
      case path'' of
        Nothing -> invalidArgs ["'path' query paramerter is required"]
        Just path' -> do
          let path = decodeUrl path'
          let isTop = path==""
          let editMe = (NewR, [("path", path'), ("mode", "e")])
          defaultLayout $ do
            setTitle $ string $ if isTop then topTitle else path
            addCassius $(cassiusFile "wiki")
            addStylesheet $ StaticR hk_kate_css
            addWidget $(widgetFile "viewNew")
    
    editNew :: Handler RepHtml
    editNew = do
      (uid, _) <- requireAuth
      path'' <- lookupGetParam "path"
      case path'' of
        Nothing -> invalidArgs ["'path' query paramerter is required"]
        Just path' -> do
          let path = decodeUrl path'
          let isTop = path==""
          defaultLayout $ do
            setTitle $ string $ if isTop then topTitle else path
            addCassius $(cassiusFile "wiki")
            addStylesheet $ StaticR hk_kate_css
            addWidget $(widgetFile "editNew")
  
postNewR :: Handler RepHtml
postNewR = do
  (uid, _) <- requireAuth
  submits@(preview, commit) <-
    uncurry (liftM2 (,)) (lookupPostParam "preview", lookupPostParam "commit")
  case submits of
    (Nothing, Nothing) -> invalidArgs ["'preview' or 'commit' parameter is required"]
    (Just _,  Just _)  -> invalidArgs ["'preview' and 'commit' parameters are alternative"]
    (Just _,  Nothing) -> previewWiki
    (Nothing, Just _)  -> createWiki
  where
    previewWiki :: Handler RepHtml
    previewWiki = do
      (uid, _) <- requireAuth
      render <- getUrlRenderParams
      params <- 
        uncurry (liftM2 (,)) (lookupPostParam "path", lookupPostParam "content")
      case params of
        (Nothing, Nothing) -> invalidArgs ["'path' and 'content' query parameters are required"]
        (Nothing,  _)      -> invalidArgs ["'path' query parameter is required"]
        (_,       Nothing) -> invalidArgs ["'content' query parameter is required"]
        (Just path', Just raw) -> do
          let path = decodeUrl path'
          pages <- runDB $ do
            pages' <- selectList [] [WikiPathAsc, WikiUpdatedDesc] 0 0
            return $ mkWikiDictionary pages'
          let pandoc = readDoc raw
          let content = preEscapedString $ writeHtmlStr render pages $ pandoc
          let isTop = path /= ""
          defaultLayout $ do
            setTitle $ string $ if isTop then topTitle else path
            addCassius $(cassiusFile "wiki")
            addStylesheet $ StaticR hk_kate_css
            addWidget $(widgetFile "previewNew")
    
    createWiki :: Handler RepHtml
    createWiki = do
      (uid, _) <- requireAuth
      params <- 
        uncurry (liftM2 (,)) (lookupPostParam "path", lookupPostParam "content")
      case params of
        (Nothing, Nothing) -> invalidArgs ["'path' and 'content' query parameters are required"]
        (Nothing,  _)      -> invalidArgs ["'path' query parameter is required"]
        (_,       Nothing) -> invalidArgs ["'content' query parameter is required"]
        (Just path', Just raw) -> do
          let path = decodeUrl path'
          now <- liftIO getCurrentTime
          runDB $ insert Wiki { 
              wikiPath=path
            , wikiContent=raw
            , wikiUpdated=now
            , wikiVersion=0
            , wikiEditor=uid
            }
          -- FIXME: use sendResponseCreated API
          redirectParams RedirectSeeOther (WikiR $ fromPath path) [("mode", "v")]
