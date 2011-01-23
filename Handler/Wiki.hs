{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Handler.Wiki where

import Kestrel

import Control.Monad
import Data.Time
import System.Locale (defaultTimeLocale)
import Control.Applicative ((<$>),(<*>))
import Web.Encodings (encodeUrl, decodeUrl)
import Data.Tuple.HT
import Data.Algorithm.Diff
import Data.List (intercalate, groupBy)
import Data.List.Split (splitOn)

import Settings (topTitle, hamletFile, cassiusFile, juliusFile, widgetFile)
import StaticFiles

getWikiListR :: Handler RepHtml
getWikiListR = do
  method <- lookupGetParam "_method"
  case method of
    Nothing -> invalidArgs [""]
    Just "search" -> searchWiki
  where
    -- pages
    searchWiki :: Handler RepHtml
    searchWiki = do
      q <- lookupGetParam "q"
      case q of
        Nothing  -> invalidArgs ["'q' parameter is required."]
        Just []  -> invalidArgs ["Please specify your search."]
        Just [x] -> invalidArgs ["Search term must be given two or more characters."]
        Just key -> do
          defaultLayout $ do
            setTitle $ string "Search Result"
            addCassius $(cassiusFile "wiki")
            addJulius $(juliusFile "wikilist")
            addStylesheet $ StaticR css_hk_kate_css
            addWidget $(widgetFile "searchWiki")

getWikiR :: WikiPage -> Handler RepHtml
getWikiR wp = do
  mode <- lookupGetParam "mode"
  case mode of
    Just "v" {-  view  page  -} -> viewWiki
    Just "e" {-  edit  page  -} -> editWiki
    Just "d" {- delete page  -} -> deleteWiki
    Just "s" {- simple page  -} -> simpleViewWiki
    Just "q" {- query  word  -} -> queryViewWiki
    Just _   {- default mode -} -> viewWiki  -- FIXME
    Nothing  {- default mode -} -> viewWiki  -- FIXME
  where
    -- Utility
    getwiki :: Handler (String, String, Html, UTCTime, Version, Maybe User, Bool)
    getwiki = do
      let path = pathOf wp
      runDB $ do
        (_, p)  <- getBy404 $ UniqueWiki path
        me <- get $ wikiEditor p
        let (raw, upd, ver) = (wikiContent p, wikiUpdated p, wikiVersion p)
            isTop = wp == topPage
        content <- markdownToWikiHtml wikiWriterOption raw
        return (path, raw, content, upd, ver, me, isTop)
    
    searchWord :: String -> String -> [Html]
    searchWord key content = pileUp $ map (search key) $ lines content
      where
        search :: String -> String -> (Bool, String)
        search word line = (found, highlighted)
          where 
            splitted = splitOn word line
            found = length splitted > 1
            highlighted = intercalate ("<span class='highlight'>"++word++"</span>") splitted
            
        pileUp :: [(Bool, String)] -> [Html]
        pileUp = map toHtml . group . remark
          where
            remark :: [(Bool, String)] -> [(Bool, String)]
            remark = id -- FIXME
            group :: [(Bool, String)] -> [[String]]
            group = map (map snd) . filter (fst.head) . groupBy (\x y -> fst x == fst y)
            toHtml :: [String] -> Html
            toHtml = preEscapedString . intercalate "<br/>"

    
    -- Pages
    queryViewWiki :: Handler RepHtml
    queryViewWiki = do
      q <- lookupGetParam "q"
      case q of
        Nothing  -> invalidArgs []
        Just key -> do
          let path =pathOf wp
          (_, p) <- runDB $ getBy404 $ UniqueWiki path
          let blocks = searchWord key $ wikiContent p
              isNull = (==[])
          hamletToRepHtml
#if GHC7
             [hamlet|
#else
             [$hamlet|
#endif
$if (not (isNull blocks))
  %fieldset.blocks
    %legend 
      %a!href=@WikiR.wp@ $path$
    $forall blocks block
      %div.block $block$
|]
        
    simpleViewWiki :: Handler RepHtml
    simpleViewWiki = do
      (_, _, content, _, _, _, _) <- getwiki
      hamletToRepHtml [$hamlet|$content$|]
    
    viewWiki :: Handler RepHtml
    viewWiki = do
      (path, raw, content, upd, ver, me, isTop) <- getwiki
      let editMe = (WikiR wp, [("mode", "e")])
          deleteMe = (WikiR wp, [("mode", "d")])
      defaultLayout $ do
        setTitle $ string $ if isTop then topTitle else path
        addCassius $(cassiusFile "wiki")
        addJulius $(juliusFile "wiki")
        addStylesheet $ StaticR css_hk_kate_css
        addWidget $(widgetFile "viewWiki")

    editWiki :: Handler RepHtml
    editWiki = do
      (uid, _) <- requireAuth
      (path, raw, content, upd, ver, _, isTop) <- getwiki
      let editMe = (WikiR wp, [("mode", "e")])
          deleteMe = (WikiR wp, [("mode", "d")])
          markdown = $(hamletFile "markdown")
      defaultLayout $ do
        setTitle $ string $ if isTop then topTitle else path
        addCassius $(cassiusFile "wiki")
        addJulius $(juliusFile "wiki")
        addStylesheet $ StaticR css_hk_kate_css
        addWidget $(widgetFile "editWiki")
            
    deleteWiki :: Handler RepHtml
    deleteWiki = do
      (uid, _) <- requireAuth
      (path, raw, content, upd, ver, me, isTop) <- getwiki
      let editMe = (WikiR wp, [("mode", "e")])
          deleteMe = (WikiR wp, [("mode", "d")])
      defaultLayout $ do
        setTitle $ string $ if isTop then topTitle else path
        addCassius $(cassiusFile "wiki")
        addJulius $(juliusFile "wiki")
        addStylesheet $ StaticR css_hk_kate_css
        addWidget $(widgetFile "deleteWiki")


postWikiR :: WikiPage -> Handler RepHtml
postWikiR wp = do
  (uid, _) <- requireAuth
  _method <- lookupPostParam "_method"
  case _method of
    Just "preview" -> previewWiki
    Just "commit"  -> putWikiR wp
    Just "delete"  -> deleteWikiR wp
    _ -> invalidArgs ["The possible values of '_method' are preview,commit,delete."]
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
      let editMe = (WikiR wp, [("mode", "e")])
          deleteMe = (WikiR wp, [("mode", "d")])
          markdown = $(hamletFile "markdown")
      defaultLayout $ do
        setTitle $ string $ if isTop then topTitle else path
        addCassius $(cassiusFile "wiki")
        addJulius $(juliusFile "wiki")
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
  runDB $ do
    (pid, page) <- getBy404 $ UniqueWiki path
    if wikiVersion page == ver
      then do
      insert WikiHistory { wikiHistoryWiki=pid
                         , wikiHistoryPath=path
                         , wikiHistoryContent=raw
                         , wikiHistoryUpdated=now
                         , wikiHistoryVersion=ver+1
                         , wikiHistoryEditor=uid
                         , wikiHistoryComment=com
                         }
      update pid [ WikiContent raw
                 , WikiUpdated now
                 , WikiVersionAdd 1
                 , WikiEditor uid
                 , WikiComment com]
      return pid
      else do
      -- FIXME Conflict?
      lift $ setMessage $ string "conflict occured. can't save your modify."
      return pid
  redirectParams RedirectSeeOther (WikiR wp) [("mode", "v")]

deleteWikiR :: WikiPage -> Handler RepHtml
deleteWikiR wp = do
  (uid, _) <- requireAuth
  let path = pathOf wp
  runDB $ do
    (pid, page) <- getBy404 $ UniqueWiki path
    deleteWhere [WikiHistoryWikiEq pid]
    delete pid
    return pid
  redirectParams RedirectSeeOther NewR [("path", encodeUrl path),("mode", "v")]

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
        Nothing -> invalidArgs ["'path' query paramerter is required."]
        Just path' -> do
          let path = decodeUrl path'
              isTop = path==Settings.topTitle
              viewMe = (NewR, [("path", path'), ("mode", "v")])
              editMe = (NewR, [("path", path'), ("mode", "e")])
          defaultLayout $ do
            setTitle $ string $ if isTop then topTitle else path
            addCassius $(cassiusFile "wiki")
            addJulius $(juliusFile "wiki")
            addStylesheet $ StaticR css_hk_kate_css
            addWidget $(widgetFile "viewNew")
    
    editNew :: Handler RepHtml
    editNew = do
      (uid, _) <- requireAuth
      path'' <- lookupGetParam "path"
      case path'' of
        Nothing -> invalidArgs ["'path' query paramerter is required."]
        Just path' -> do
          let path = decodeUrl path'
              isTop = path==Settings.topTitle
              viewMe = (NewR, [("path", path'), ("mode", "v")])
              editMe = (NewR, [("path", path'), ("mode", "e")])
              markdown = $(hamletFile "markdown")
          defaultLayout $ do
            setTitle $ string $ if isTop then topTitle else path
            addCassius $(cassiusFile "wiki")
            addJulius $(juliusFile "wiki")
            addStylesheet $ StaticR css_hk_kate_css
            addWidget $(widgetFile "editNew")
  
postNewR :: Handler RepHtml
postNewR = do
  (uid, _) <- requireAuth
  _method <- lookupPostParam "_method"
  case _method of
    Just "preview" -> previewWiki
    Just "commit"  -> createWiki
    _              -> invalidArgs ["The possible values of '_method' are preview,commit."]
  where
    previewWiki :: Handler RepHtml
    previewWiki = do
      (uid, _) <- requireAuth
      (path', raw, com) <- runFormPost' $ (,,)
                           <$> stringInput "path"
                           <*> stringInput "content"
                           <*> maybeStringInput "comment"
      let path = decodeUrl path'
          isTop = path == Settings.topTitle
          viewMe = (NewR, [("path", path'), ("mode", "v")])
          editMe = (NewR, [("path", path'), ("mode", "e")])
          markdown = $(hamletFile "markdown")
      content <- runDB $ markdownToWikiHtml wikiWriterOption raw
      defaultLayout $ do
        setTitle $ string $ if isTop then topTitle else path
        addCassius $(cassiusFile "wiki")
        addJulius $(juliusFile "wiki")
        addStylesheet $ StaticR css_hk_kate_css
        addWidget $(widgetFile "previewNew")
    
    createWiki :: Handler RepHtml
    createWiki = do
      (uid, _) <- requireAuth
      (path', raw, com) <- runFormPost' $ (,,)
                           <$> stringInput "path"
                           <*> stringInput "content"
                           <*> maybeStringInput "comment"
      let path = decodeUrl path'
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
        }
        -- FIXME: use sendResponseCreated API
      redirectParams RedirectSeeOther (WikiR $ fromPath path) [("mode", "v")]

getHistoriesR :: WikiPage -> Handler RepHtml
getHistoriesR wp = do
  ver <- lookupGetParam "ver"
  case ver of
    Nothing -> historyList $ -1
    Just v  -> historyList $ read v
  where
    -- Utility
    getHistories :: Handler [(User, WikiHistory)]
    getHistories = do
      let path = pathOf wp
      runDB $ do
        (pid, _) <- getBy404 $ UniqueWiki path
        hists' <- selectList [WikiHistoryWikiEq pid] [WikiHistoryVersionDesc] 0 0
        hists <- forM hists' $ \(hid, h) -> do
          Just u <- get $ wikiHistoryEditor h
          return (u, h)
        return hists
    
    mkHistsWithDiff :: [(User, WikiHistory)] -> [(User, WikiHistory, (Version, Version))]
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
    
    -- pages
    historyList :: Version -> Handler RepHtml
    historyList v = do
      mu <- maybeAuth
      let path = pathOf wp
          isTop = wp == topPage
          isNull = (""==)
          pagingSize = 25
      hs'' <- getHistories
      let hs' = mkHistsWithDiff hs''
          v' = if v >= 0 then v else curver
          hs = take pagingSize $ drop (max (curver-v') 0) hs'
          curver = (wikiHistoryVersion.snd.head) hs''
          editMe = (WikiR wp, [("mode", "e")])
          deleteMe = (WikiR wp, [("mode", "d")])
          viewVer = \v -> (HistoryR v wp, [("mode", "v")])
          editVer = \v -> (HistoryR v wp, [("mode", "e")])
          revertVer = \v -> (HistoryR v wp, [("mode", "r")])
          prevDiff = \v -> (HistoryR v wp, [("mode", "p")])
          currDiff = \v -> (HistoryR v wp, [("mode", "c")])
          notCurrent = \h -> wikiHistoryVersion h /= curver
          notEpoch = \h -> wikiHistoryVersion h /= 0
          canDiff = \h -> notCurrent h || notEpoch h
          canDiff2 = \h -> notCurrent h && notEpoch h
          showDate = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"
          altClass = \h -> if wikiHistoryVersion h `mod` 2 == 0
                           then "even"::String
                           else "odd"
          mnext = if v' >= pagingSize
                  then Just (HistoriesR wp, [("ver", show $ v'-pagingSize)])
                  else Nothing
      defaultLayout $ do
        setTitle $ string $ if isTop then topTitle else path
        addCassius $(cassiusFile "wiki")
        addJulius $(juliusFile "wiki")
        addWidget $(widgetFile "listHistories")

        

getHistoryR :: Version -> WikiPage -> Handler RepHtml
getHistoryR v wp = do
  mode <- lookupGetParam "mode"
  case mode of
    Just "v" {-       view       -} -> viewHistory v
    Just "e" {-       edit       -} -> editHistory v
    Just "p" {- diff to previous -} -> diffPrevious v
    Just "c" {- diff to current  -} -> diffCurrent v
    Just "r" {-      revert      -} -> revertHistory v
    _        {-      illegal     -} -> invalidArgs ["The possible values of 'mode' are l,v,e,p,c,r."]
  where
    -- Utility
    getHistory :: Version -> Handler (String, String, Html, UTCTime, Version, Maybe User, Bool, Wiki)
    getHistory v = do
      let path = pathOf wp
      runDB $ do
        (pid', p') <- getBy404 $ UniqueWiki path
        (pid, p) <- getBy404 $ UniqueWikiHistory pid' v
        me <- get $ wikiHistoryEditor p
        let (raw, upd, ver) = (wikiHistoryContent p, wikiHistoryUpdated p, wikiHistoryVersion p)
            isTop = wp == topPage
        content <- markdownToWikiHtml wikiWriterOption raw
        return (path, raw, content, upd, ver, me, isTop, p')

    getHistories :: Handler [(User, WikiHistory)]
    getHistories = do
      let path = pathOf wp
      runDB $ do
        (pid, _) <- getBy404 $ UniqueWiki path
        hists' <- selectList [WikiHistoryWikiEq pid] [WikiHistoryVersionDesc] 0 0
        hists <- forM hists' $ \(hid, h) -> do
          Just u <- get $ wikiHistoryEditor h
          return (u, h)
        return hists
            
    mkDiff :: WikiHistory -> WikiHistory -> Html
    mkDiff new old = preEscapedString $ foldr d2h "" diffs
      where
        diffs = getDiff (lines' new) (lines' old)
        lines' = lines . wikiHistoryContent
        d2h (F, l) xs = "<span class='plus'>+&nbsp;" ++ l ++ "</span><br/>" ++ xs
        d2h (S, l) xs = "<span class='minus'>-&nbsp;" ++ l ++ "</span><br/>" ++ xs
        d2h (B, l) xs = "<span>&nbsp;&nbsp;" ++ l ++ "</span><br/>" ++ xs
        
    -- pages
    viewHistory :: Version -> Handler RepHtml
    viewHistory v = do
      (path, raw, content, upd, _, me, isTop, curp) <- getHistory v
      let editMe = (WikiR wp, [("mode", "e")])
          deleteMe = (WikiR wp, [("mode", "d")])
          ver = wikiVersion curp
          notCurrent =  v /= ver
          editVer = (HistoryR v wp, [("mode", "e")])
          currDiff = (HistoryR v wp, [("mode", "c")])
      defaultLayout $ do
        setTitle $ string $ if isTop then topTitle else path
        addCassius $(cassiusFile "wiki")
        addJulius $(juliusFile "wiki")
        addStylesheet $ StaticR css_hk_kate_css
        addWidget $(widgetFile "viewHistory")

    editHistory :: Version -> Handler RepHtml
    editHistory v = do
      (uid, _) <- requireAuth
      (path, raw, content, upd, _, me, isTop, curp) <- getHistory v
      let editMe = (WikiR wp, [("mode", "e")])
          deleteMe = (WikiR wp, [("mode", "d")])
          ver = wikiVersion curp
          notCurrent =  v /= ver
          markdown = $(hamletFile "markdown")
      defaultLayout $ do
        setTitle $ string $ if isTop then topTitle else path
        addCassius $(cassiusFile "wiki")
        addJulius $(juliusFile "wiki")
        addStylesheet $ StaticR css_hk_kate_css
        addWidget $(widgetFile "editHistory")
    
    revertHistory :: Version -> Handler RepHtml
    revertHistory v = do
      (uid, _) <- requireAuth
      (path, raw, content, upd, _, me, isTop, curp) <- getHistory v
      let editMe = (WikiR wp, [("mode", "e")])
          deleteMe = (WikiR wp, [("mode", "d")])
          ver = wikiVersion curp
          notCurrent =  v /= ver
          editVer = (HistoryR v wp, [("mode", "e")])
      defaultLayout $ do
        setTitle $ string $ if isTop then topTitle else path
        addCassius $(cassiusFile "wiki")
        addJulius $(juliusFile "wiki")
        addStylesheet $ StaticR css_hk_kate_css
        addWidget $(widgetFile "revertHistory")

    diffVers :: (Wiki -> Version -> [Version]) -> Version -> Handler RepHtml
    diffVers selver v = do
      let path = pathOf wp
      (p, v1, v0) <- runDB $ do
        (pid, p) <- getBy404 $ UniqueWiki path
        [(_, v1),(_, v0)] <- 
          selectList [WikiHistoryWikiEq pid, WikiHistoryVersionIn (selver p v)]
                     [WikiHistoryVersionDesc] 2 0
        return (p, v1, v0)
      let editMe = (WikiR wp, [("mode", "e")])
          deleteMe = (WikiR wp, [("mode", "d")])
          title = if wikiVersion p == wikiHistoryVersion v1
                  then "updated from " ++ showDate (wikiHistoryUpdated v0)
                  else "updated between " ++ showDate (wikiHistoryUpdated v0) ++ " to " ++ showDate (wikiHistoryUpdated v1)
          content = mkDiff v1 v0
          isTop = wp == topPage
      defaultLayout $ do
        setTitle $ string $ if isTop then topTitle else path
        addCassius $(cassiusFile "wiki")
        addJulius $(juliusFile "wiki")
        addWidget $(widgetFile "diffHistories")
    
    diffPrevious :: Version -> Handler RepHtml
    diffPrevious = diffVers $ \p v -> [v, v-1]
    
    diffCurrent :: Version -> Handler RepHtml
    diffCurrent = diffVers $ \p v -> [wikiVersion p, v]


postHistoryR :: Version -> WikiPage -> Handler RepHtml
postHistoryR v wp = do
  (uid, _) <- requireAuth
  _method <- lookupPostParam "_method"
  case _method of
    Just "preview" -> previewHistory
    Just "commit"  -> putWikiR wp
    Just "modify"  -> putHistoryR v wp
    _              -> invalidArgs ["The possible values of '_method' are preview,commit,modify", "hogehoge."]
  where
    
    previewHistory :: Handler RepHtml
    previewHistory = do
      let path = pathOf wp
          isTop = wp == topPage
      (raw, com, ver, v) <- runFormPost' $ (,,,)
                            <$> stringInput "content"
                            <*> maybeStringInput "comment"
                            <*> intInput "version"
                            <*> intInput "original_version"
      content <- runDB $ markdownToWikiHtml wikiWriterOption raw
      let editMe = (WikiR wp, [("mode", "e")])
          deleteMe = (WikiR wp, [("mode", "d")])
          notCurrent = v /= ver
          markdown = $(hamletFile "markdown")
      defaultLayout $ do
        setTitle $ string $ if isTop then topTitle else path
        addCassius $(cassiusFile "wiki")
        addJulius $(juliusFile "wiki")
        addStylesheet $ StaticR css_hk_kate_css
        addWidget $(widgetFile "previewHistory")

putHistoryR :: Version -> WikiPage -> Handler RepHtml
putHistoryR v wp = do
  (uid, _) <- requireAuth
  let path = pathOf wp
  com <- runFormPost' $ maybeStringInput "comment"
  runDB $ do
    (pid, _) <- getBy404 $ UniqueWiki path
    (hid, h) <- getBy404 $ UniqueWikiHistory pid v
    if uid == wikiHistoryEditor h
      then update hid [ WikiHistoryComment com ]
      else lift $ invalidArgs ["You couldn't modify the history editted by the others."]
  hamletToRepHtml
#if GHC7
    [hamlet|
#else
    [$hamlet|
#endif
$maybe com c
  %span $c$
$nothing
  %span *** no log comment ***
|]
