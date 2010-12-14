{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Wiki where

import Kestrel
import Kestrel.WikiParser

import Control.Monad
import Data.Time
import Control.Applicative ((<$>),(<*>))
import Web.Encodings (encodeUrl, decodeUrl)

import StaticFiles

getWikiR :: WikiPage -> Handler RepHtml
getWikiR wp = do
  mode <- lookupGetParam "mode"
  case mode of
    Just "v" {-  view  page  -} -> viewWiki
    Just "e" {-  edit  page  -} -> editWiki
    Just "d" {- delete page  -} -> deleteWiki
    Just _   {- default mode -} -> viewWiki  -- FIXME
    Nothing  {- default mode -} -> viewWiki  -- FIXME
  where
    -- Utility
    getwiki :: Handler (Maybe (String, String, Html, UTCTime, Int, Maybe User, Bool))
    getwiki = do
      let path = pathOf wp
      runDB $ do
        page'  <- getBy $ UniqueWiki path
        case page' of
          Nothing -> return Nothing
          Just (_, p) -> do
            me <- get $ wikiEditor p
            let (raw, upd, ver) = (wikiContent p, wikiUpdated p, wikiVersion p)
            content <- markdownToWikiHtml wikiWriterOption raw
            let isTop = wp == topPage
            return $ Just (path, raw, content, upd, ver, me, isTop)
    
    -- Pages
    viewWiki :: Handler RepHtml
    viewWiki = do
      wiki <- getwiki
      case wiki of
        Nothing -> notFound
        Just (path, raw, content, upd, ver, me, isTop) -> do
          let editMe = (WikiR wp, [("mode", "e")])
          let deleteMe = (WikiR wp, [("mode", "d")])
          defaultLayout $ do
            setTitle $ string $ if isTop then topTitle else path
            addCassius $(cassiusFile "wiki")
            addStylesheet $ StaticR css_hk_kate_css
            addWidget $(widgetFile "viewWiki")

    editWiki :: Handler RepHtml
    editWiki = do
      (uid, _) <- requireAuth
      wiki <- getwiki
      case wiki of
        Nothing -> notFound
        Just (path, raw, content, upd, ver, editor, isTop) -> do
          let deleteMe = (WikiR wp, [("mode", "d")])
          defaultLayout $ do
            setTitle $ string $ if isTop then topTitle else path
            addCassius $(cassiusFile "wiki")
            addStylesheet $ StaticR css_hk_kate_css
            addWidget $(widgetFile "editWiki")
            
    deleteWiki :: Handler RepHtml
    deleteWiki = do
      (uid, _) <- requireAuth
      wiki <- getwiki
      case wiki of
        Nothing -> notFound
        Just (path, raw, content, upd, ver, me, isTop) -> do
          let editMe = (WikiR wp, [("mode", "e")])
          defaultLayout $ do
            setTitle $ string $ if isTop then topTitle else path
            addCassius $(cassiusFile "wiki")
            addStylesheet $ StaticR css_hk_kate_css
            addWidget $(widgetFile "deleteWiki")


postWikiR :: WikiPage -> Handler RepHtml
postWikiR wp = do
  (uid, _) <- requireAuth
  submits@(preview, commit, delete) <-
    uncurry3 (liftM3 (,,)) (lookupPostParam "preview", lookupPostParam "commit", lookupPostParam "delete")
  case submits of
    (Just _,  Nothing, Nothing) -> previewWiki
    (Nothing, Just _ , Nothing) -> putWikiR wp
    (Nothing, Nothing, Just _ ) -> deleteWikiR wp
    _ -> invalidArgs ["'preview' or 'commit' or 'delete' parameter is required"]
  where
    uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
    uncurry3 f (x, y, z) = f x y z
    
    previewWiki :: Handler RepHtml
    previewWiki = do
      let path = pathOf wp
      (raw, ver) <- runFormPost' $ (,)
                         <$> stringInput "content"
                         <*> intInput "version"
      com <- lookupPostParam "comment"
      content <- runDB $ markdownToWikiHtml wikiWriterOption raw
      let isTop = wp == topPage
      let deleteMe = (WikiR wp, [("mode", "d")])
      defaultLayout $ do
        setTitle $ string $ if isTop then topTitle else path
        addCassius $(cassiusFile "wiki")
        addStylesheet $ StaticR css_hk_kate_css
        addWidget $(widgetFile "previewWiki")

putWikiR :: WikiPage -> Handler RepHtml
putWikiR wp = do
  (uid, _) <- requireAuth
  let path = pathOf wp
  now <- liftIO getCurrentTime
  (raw, ver) <- runFormPost' $ (,)
                     <$> stringInput "content"
                     <*> intInput "version"
  com <- lookupPostParam "comment"
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
            , wikiHistoryComment=(wikiComment page)                                
            , wikiHistoryDeleted=False
            }
          update pid [ WikiContent raw
                     , WikiUpdated now
                     , WikiVersionAdd 1
                     , WikiEditor uid
                     , WikiComment com]
          return $ Just pid
          else do
          -- FIXME Conflict?
          lift $ setMessage $ string "conflict occured. can't save your modify."
          return $ Just pid
  case id of
    Nothing -> notFound -- FIXME invalidArgs?
    Just _ -> redirectParams RedirectSeeOther (WikiR wp) [("mode", "v")]

deleteWikiR :: WikiPage -> Handler RepHtml
deleteWikiR wp = do
  (uid, _) <- requireAuth
  let path = pathOf wp
  id <- runDB $ do
    wiki <- getBy $ UniqueWiki path
    case wiki of
      Nothing -> return Nothing
      Just (pid, page) -> do
        deleteWhere [WikiHistoryWikiEq pid]
        delete pid
        return $ Just pid
  case id of
    Nothing -> notFound -- FIXME invalidArgs?
    Just _ -> redirectParams RedirectSeeOther NewR [("path", encodeUrl path),("mode", "v")]

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
            addStylesheet $ StaticR css_hk_kate_css
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
          let viewMe = (NewR, [("path", path'), ("mode", "v")])
          defaultLayout $ do
            setTitle $ string $ if isTop then topTitle else path
            addCassius $(cassiusFile "wiki")
            addStylesheet $ StaticR css_hk_kate_css
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
      (path', raw) <- runFormPost' $ (,)
                          <$> stringInput "path"
                          <*> stringInput "content"
      com <- lookupPostParam "comment"
      let path = decodeUrl path'
      content <- runDB $ markdownToWikiHtml wikiWriterOption raw
      let isTop = path == ""
      let viewMe = (NewR, [("path", path'), ("mode", "v")])
      defaultLayout $ do
        setTitle $ string $ if isTop then topTitle else path
        addCassius $(cassiusFile "wiki")
        addStylesheet $ StaticR css_hk_kate_css
        addWidget $(widgetFile "previewNew")
    
    createWiki :: Handler RepHtml
    createWiki = do
      (uid, _) <- requireAuth
      (path', raw) <- runFormPost' $ (,)
                          <$> stringInput "path"
                          <*> stringInput "content"
      com <- lookupPostParam "comment"
      let path = decodeUrl path'
      now <- liftIO getCurrentTime
      runDB $ insert Wiki { 
          wikiPath=path
        , wikiContent=raw
        , wikiUpdated=now
        , wikiVersion=0
        , wikiEditor=uid
        , wikiComment=com
        }
        -- FIXME: use sendResponseCreated API
      redirectParams RedirectSeeOther (WikiR $ fromPath path) [("mode", "v")]
