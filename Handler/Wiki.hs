{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Wiki where

import Kestrel
import Kestrel.WikiParser

import Control.Monad
import Data.Time
import System.Locale (defaultTimeLocale)
import Control.Applicative ((<$>),(<*>))
import Web.Encodings (encodeUrl, decodeUrl)
import Data.Tuple.HT
import Data.Algorithm.Diff

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
                isTop = wp == topPage
            content <- markdownToWikiHtml wikiWriterOption raw
            return $ Just (path, raw, content, upd, ver, me, isTop)
    
    -- Pages
    viewWiki :: Handler RepHtml
    viewWiki = do
      wiki <- getwiki
      case wiki of
        Nothing -> notFound
        Just (path, raw, content, upd, ver, me, isTop) -> do
          let editMe = (WikiR wp, [("mode", "e")])
              deleteMe = (WikiR wp, [("mode", "d")])
              myHistory = (HistoryR wp, [("mode", "l"),("ver", show ver)])
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
              myHistory = (HistoryR wp, [("mode", "l"),("ver", show ver)])
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
              myHistory = (HistoryR wp, [("mode", "l"),("ver", show ver)])
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
    
    previewWiki :: Handler RepHtml
    previewWiki = do
      let path = pathOf wp
          isTop = wp == topPage
      (raw, com, ver) <- runFormPost' $ (,,)
                         <$> stringInput "content"
                         <*> maybeStringInput "comment"
                         <*> intInput "version"
      content <- runDB $ markdownToWikiHtml wikiWriterOption raw
      let deleteMe = (WikiR wp, [("mode", "d")])
          myHistory = (HistoryR wp, [("mode", "l"),("ver", show ver)])
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
  (raw, com, ver) <- runFormPost' $ (,,)
                     <$> stringInput "content"
                     <*> maybeStringInput "comment"
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
            , wikiHistoryPath=path
            , wikiHistoryContent=raw
            , wikiHistoryUpdated=now
            , wikiHistoryVersion=ver+1
            , wikiHistoryEditor=uid
            , wikiHistoryComment=com
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
              isTop = path==""
              editMe = (NewR, [("path", path'), ("mode", "e")])
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
              isTop = path==""
              viewMe = (NewR, [("path", path'), ("mode", "v")])
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
      (path'', raw, com) <- runFormPost' $ (,,)
                           <$> maybeStringInput "path"
                           <*> stringInput "content"
                           <*> maybeStringInput "comment"
      let path' = case path'' of
            Nothing -> "" -- TOP Page
            Just p -> p
          path = decodeUrl path'
          isTop = path == ""
          viewMe = (NewR, [("path", path'), ("mode", "v")])
      content <- runDB $ markdownToWikiHtml wikiWriterOption raw
      defaultLayout $ do
        setTitle $ string $ if isTop then topTitle else path
        addCassius $(cassiusFile "wiki")
        addStylesheet $ StaticR css_hk_kate_css
        addWidget $(widgetFile "previewNew")
    
    createWiki :: Handler RepHtml
    createWiki = do
      (uid, _) <- requireAuth
      (path'', raw, com) <- runFormPost' $ (,,)
                           <$> maybeStringInput "path"
                           <*> stringInput "content"
                           <*> maybeStringInput "comment"
      let path' = case path'' of
            Nothing -> ""
            Just p -> p
          path = decodeUrl path'
      now <- liftIO getCurrentTime
      runDB $ do
        pid <- insert Wiki { 
          wikiPath=path
        , wikiContent=raw
        , wikiUpdated=now
        , wikiVersion=0
        , wikiEditor=uid
        , wikiComment=com
        }
        insert WikiHistory {
          wikiHistoryWiki=pid
        , wikiHistoryPath=path
        , wikiHistoryContent=raw
        , wikiHistoryUpdated=now
        , wikiHistoryVersion=0
        , wikiHistoryEditor=uid
        , wikiHistoryComment=com
        , wikiHistoryDeleted=False
        }
        -- FIXME: use sendResponseCreated API
      redirectParams RedirectSeeOther (WikiR $ fromPath path) [("mode", "v")]

getHistoryR :: WikiPage -> Handler RepHtml
getHistoryR wp = do
  params@(mode, ver) <- uncurry (liftM2 (,)) 
                        (lookupGetParam "mode", lookupGetParam "ver")
  case params of
    (Just "l", Just v ) {-       list       -} -> historyList $ read v
    (Just "v", Just v ) {-       view       -} -> viewHistory $ read v
    (Just "e", Just v ) {-       edit       -} -> editHistory $ read v
    (Just "p", Just v ) {- diff to previous -} -> diffPrevious $ read v
    (Just "c", Just v ) {- diff to current  -} -> diffCurrent $ read v
    (Just "r", Just v ) {-      revert      -} -> revertHistory $ read v
    _                   {-      illegal     -} -> invalidArgs ["'mode' and 'ver' parameters are required."]
  where
    -- Utility
    getHistory :: Int -> Handler (Maybe (String, String, Html, UTCTime, Int, Maybe User, Bool, Wiki))
    getHistory v = do
      let path = pathOf wp
      runDB $ do
        page'  <- getBy $ UniqueWiki path
        case page' of
          Nothing -> return Nothing
          Just (pid', p') -> do
            [(pid, p)] <- selectList [WikiHistoryWikiEq pid', WikiHistoryVersionEq v] [] 0 0
            me <- get $ wikiHistoryEditor p
            let (raw, upd, ver) = (wikiHistoryContent p, wikiHistoryUpdated p, wikiHistoryVersion p)
                isTop = wp == topPage
            content <- markdownToWikiHtml wikiWriterOption raw
            return $ Just (path, raw, content, upd, ver, me, isTop, p')

    getHistories :: Handler (Maybe [(User, WikiHistory)])
    getHistories = do
      let path = pathOf wp
      runDB $ do
        page' <- getBy $ UniqueWiki path
        case page' of
          Nothing -> return Nothing
          Just (pid, _) -> do
            hists' <- selectList [WikiHistoryWikiEq pid] [WikiHistoryVersionDesc] 0 0
            hists <- forM hists' $ \(hid, h) -> do
              Just u <- get $ wikiHistoryEditor h
              return (u, h)
            return $ Just hists
            
    mkHistsWithDiff :: [(User, WikiHistory)] -> [(User, WikiHistory, (Int, Int))]
    mkHistsWithDiff hs = zipWith p2t hs diffs
      where 
        p2t :: (a, b) -> c -> (a, b, c)
        p2t (x, y) z = (x, y, z)
        new = map (lines . wikiHistoryContent . snd) hs
        old = tail new ++ [[]]
        diffs = zipWith ((foldr dc (0,0).).getDiff) new old
        dc (F,_) (f,s) = (f+1,s)
        dc (S,_) (f,s) = (f,s-1)
        dc _     fs    = fs
    
    mkDiff :: WikiHistory -> WikiHistory -> Html
    mkDiff new old = preEscapedString $ foldr d2h "" diffs
      where
        diffs = getDiff (lines' new) (lines' old)
        lines' = lines . wikiHistoryContent
        d2h (F, l) xs = "<span class='plus'>+&nbsp;" ++ l ++ "</span><br/>" ++ xs
        d2h (S, l) xs = "<span class='minus'>-&nbsp;" ++ l ++ "</span><br/>" ++ xs
        d2h (B, l) xs = "<span>&nbsp;&nbsp;" ++ l ++ "</span><br/>" ++ xs
        
    -- pages
    historyList :: Int -> Handler RepHtml
    historyList v = do
      let path = pathOf wp
          isTop = wp == topPage
          isNull = (""==)
      hists' <- getHistories
      case hists' of
        Nothing -> notFound -- FIXME
        Just hs'' -> do
          let hs' = mkHistsWithDiff hs''
              hs = take 25 $ drop (max (curver-v) 0) hs'
              curver = (wikiHistoryVersion.snd.head) hs''
              editMe = (WikiR wp, [("mode", "e")])
              deleteMe = (WikiR wp, [("mode", "d")])
              viewVer = \v -> (HistoryR wp, [("mode", "v"),("ver", show v)])
              editVer = \v -> (HistoryR wp, [("mode", "e"),("ver", show v)])
              revertVer = \v -> (HistoryR wp, [("mode", "r"),("ver", show v)])
              prevDiff = \v -> (HistoryR wp, [("mode", "p"),("ver", show v)])
              currDiff = \v -> (HistoryR wp, [("mode", "c"),("ver", show v)])
              notCurrent = \h -> wikiHistoryVersion h /= curver
              notEpoch = \h -> wikiHistoryVersion h /= 0
              canDiff = \h -> notCurrent h || notEpoch h
              canDiff2 = \h -> notCurrent h && notEpoch h
              showDate = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"
              altClass = \h -> if wikiHistoryVersion h `mod` 2 == 0
                               then "even"::String
                               else "odd"
              mnext = if v >= 25
                      then Just (HistoryR wp, [("mode","l"),("ver", show $ v-25)])
                      else Nothing
          defaultLayout $ do
            setTitle $ string $ if isTop then topTitle else path
            addCassius $(cassiusFile "wiki")
            addWidget $(widgetFile "listHistories")
    -- TODO
    viewHistory :: Int -> Handler RepHtml
    viewHistory v = do
      wiki <- getHistory v
      case wiki of
        Nothing -> notFound
        Just (path, raw, content, upd, ver, me, isTop, curp) -> do
          let editMe = (WikiR wp, [("mode", "e")])
              deleteMe = (WikiR wp, [("mode", "d")])
              myHistory = (HistoryR wp, [("mode", "l"),("ver", show $ wikiVersion curp)])
          defaultLayout $ do
            setTitle $ string $ if isTop then topTitle else path
            addCassius $(cassiusFile "wiki")
            addStylesheet $ StaticR css_hk_kate_css
            addWidget $(widgetFile "viewHistory")

    -- TODO
    editHistory :: Int -> Handler RepHtml
    editHistory = undefined
    -- TODO
    revertHistory :: Int -> Handler RepHtml
    revertHistory v = undefined

    diffVers :: (Wiki -> Int -> [Int]) -> Int -> Handler RepHtml
    diffVers selver v = do
      let path = pathOf wp
      hists' <- runDB $ do
        page' <- getBy $ UniqueWiki path
        case page' of
          Nothing -> return Nothing
          Just (pid, p) -> do
            [(_, v1),(_, v0)] <- 
              selectList [WikiHistoryWikiEq pid, WikiHistoryVersionIn (selver p v)]
                         [WikiHistoryVersionDesc] 2 0
            return $ Just (p, v1, v0)
      case hists' of
        Nothing -> notFound -- FIXME
        Just (p, v1, v0) -> do
          let editMe = (WikiR wp, [("mode", "e")])
              deleteMe = (WikiR wp, [("mode", "d")])
              myHistory = (HistoryR wp, [("mode", "l"),("ver", show $ wikiVersion p)])
              title = if wikiVersion p == wikiHistoryVersion v1
                      then "updated from " ++ showDate (wikiHistoryUpdated v0)
                      else "updated between " ++ showDate (wikiHistoryUpdated v0) ++ " to " ++ showDate (wikiHistoryUpdated v1)
              content = mkDiff v1 v0
              isTop = wp == topPage
          defaultLayout $ do
            setTitle $ string $ if isTop then topTitle else path
            addCassius $(cassiusFile "wiki")
            addWidget $(widgetFile "diffHistories")
    
    diffPrevious :: Int -> Handler RepHtml
    diffPrevious = diffVers $ \p v -> [v, v-1]
    
    diffCurrent :: Int -> Handler RepHtml
    diffCurrent = diffVers $ \p v -> [wikiVersion p, v]
    