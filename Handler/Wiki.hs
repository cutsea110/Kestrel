{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-} 
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Handler.Wiki where

import Foundation

import Yesod
import Control.Monad
import Data.Time
import Control.Applicative ((<$>),(<*>))
import Data.Tuple.HT
import Data.Algorithm.Diff
import Data.List (groupBy)
import Data.Text (Text)
import Data.String (IsString)
import qualified Data.Text as T
import Text.Blaze.Internal (preEscapedText)
import Text.Cassius (cassiusFile)
import Text.Julius (juliusFile)

import Settings (topTitle)
import Settings.StaticFiles

getDoc :: (IsString t, Eq t) => [t] -> GWidget sub master ()
getDoc [] = $(whamletFile "templates/markdown-help-en.hamlet")
getDoc ("en":_) = $(whamletFile "templates/markdown-help-en.hamlet")
getDoc ("ja":_) = $(whamletFile "templates/markdown-help-ja.hamlet")
getDoc (_:ds) = getDoc ds

getEitherWikiNewR :: WikiPage -> Handler RepHtml
getEitherWikiNewR wp = do
  let path = pathOf wp
  mwiki <- runDB $ getBy $ UniqueWiki path
  case mwiki of
    Nothing -> 
      redirect (NewR, [("path", path), ("mode", "v")] :: [(Text, Text)])
    Just _ ->
      redirect $ WikiR wp

getWikiListR :: Handler RepHtml
getWikiListR = do
  method <- lookupGetParam "_method"
  case method of
    Just "search" -> searchWiki
    _ -> invalidArgs ["The possible values of '_method' is search."]
  where
    -- pages
    searchWiki :: Handler RepHtml
    searchWiki = do
      q <- lookupGetParam "q"
      case q of
        Nothing  -> invalidArgs ["'q' parameter is required."]
        Just key -> case T.length key of
          0 -> invalidArgs ["Please specify your search."]
          1 -> invalidArgs ["Search term must be given two or more characters."]
          _ -> defaultLayout $ do
            setTitle $ preEscapedText "Search Result"
            toWidget $(cassiusFile "templates/wiki.cassius")
            toWidget $(juliusFile "templates/wikilist.julius")
            addStylesheet $ StaticR css_hk_kate_css
            $(whamletFile "templates/searchWiki.hamlet")

getAllPagesR :: Handler RepHtmlJson
getAllPagesR = do
  render <- getUrlRender
  entries <- runDB $ selectList [] [Asc WikiPath]
  let widget = $(widgetFile "allPages")
      json = object ["entries" .= array (map (go render) entries)]
  defaultLayoutJson widget json
  where
    go r (Entity _ w) =
      object [ "title" .= wikiPath w
             , "uri" .= r (WikiR (fromPath (wikiPath w)))
             , "uday" .= wikiUpdated w
             ]


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
    getwiki :: WriterOptions -> Handler (Text, Text, Html, UTCTime, Version, Maybe User, Bool)
    getwiki opt = do
      let path = pathOf wp
      runDB $ do
        (Entity _ p)  <- getBy404 $ UniqueWiki path
        me <- get $ wikiEditor p
        let (raw, upd, ver) = (wikiContent p, wikiUpdated p, wikiVersion p)
            isTop = wp == topPage
        content <- markdownToWikiHtml opt raw
        return (path, raw, content, upd, ver, me, isTop)
    
    searchWord :: Text -> Text -> [Html]
    searchWord key content = pileUp $ map (search key) $ T.lines content
      where
        search :: Text -> Text -> (Bool, Text)
        search word line = (found, highlighted)
          where 
            splitted = T.splitOn word line
            found = length splitted > 1
            highlighted = T.intercalate ("<span class='highlight'>"+++word+++"</span>") splitted
            
        pileUp :: [(Bool, Text)] -> [Html]
        pileUp = map toHtml' . group . remark 3
          where
            remark :: Int -> [(Bool, Text)] -> [(Bool, Text)]
            remark 1 xs = transmit xs shiftL shiftR
              where
                shiftL = drop 1 xs ++ [(False, undefined)]
                shiftR = (False, undefined):xs
                transmit :: [(Bool, Text)] -> [(Bool, Text)] -> [(Bool, Text)] -> [(Bool, Text)]
                transmit ((o,os):os') ((l,_):ls') ((r,_):rs') = (or [o,l,r], os):transmit os' ls' rs'
                transmit _ _ _ = []
            remark n xs = remark 1 $ remark (n-1) xs
                
            group :: [(Bool, Text)] -> [[Text]]
            group = map (map snd) . filter (fst.head) . groupBy (\x y -> fst x == fst y)
            toHtml' :: [Text] -> Html
            toHtml' = preEscapedText . T.intercalate "<br/>"

    
    -- Pages
    queryViewWiki :: Handler RepHtml
    queryViewWiki = do
      q <- lookupGetParam "q"
      case q of
        Nothing  -> invalidArgs []
        Just key -> do
          let path =pathOf wp
          (Entity _ p) <- runDB $ getBy404 $ UniqueWiki path
          let blocks = searchWord key $ wikiContent p
              isNull = \h -> case h of
                [] -> True
                _  -> False
          hamletToRepHtml
             [hamlet|$newline never
$if not (isNull blocks)
  <fieldset .blocks>
    <legend>
      <a href="@{WikiR wp}">#{path}
    $forall block <- blocks
      <div .block>#{block}
|]
        
    simpleViewWiki :: Handler RepHtml
    simpleViewWiki = do
      (_, _, content, _, _, _, _) <- getwiki sidePaneWriterOption
      hamletToRepHtml [hamlet|$newline never
\#{content}
|]
    
    viewWiki :: Handler RepHtml
    viewWiki = do
      msgShow <- getMessageRender
      (path, raw, content, upd, ver, me, isTop) <- getwiki (wikiWriterOption msgShow)
      let editMe = (WikiR wp, [("mode", "e")])
          deleteMe = (WikiR wp, [("mode", "d")])
      defaultLayout $ do
        setTitle $ preEscapedText path
        $(widgetFile "wiki")
        addStylesheet $ StaticR css_hk_kate_css
        $(whamletFile "templates/viewWiki.hamlet")

    editWiki :: Handler RepHtml
    editWiki = do
      (Entity uid _) <- requireAuth
      msgShow <- getMessageRender
      (path, raw, content, upd, ver, _, isTop) <- getwiki (wikiWriterOption msgShow)
      langs <- languages
      let editMe = (WikiR wp, [("mode", "e")])
          deleteMe = (WikiR wp, [("mode", "d")])
          markdown = getDoc langs
          toBool = maybe False (const True)
          donttouch = if isSidePane path then Just undefined else Nothing
      defaultLayout $ do
        setTitle $ preEscapedText path
        $(widgetFile "wiki")
        addStylesheet $ StaticR css_hk_kate_css
        $(whamletFile "templates/editWiki.hamlet")
            
    deleteWiki :: Handler RepHtml
    deleteWiki = do
      (Entity uid _) <- requireAuth
      msgShow <- getMessageRender
      (path, raw, content, upd, ver, me, isTop) <- getwiki (wikiWriterOption msgShow)
      let editMe = (WikiR wp, [("mode", "e")])
          deleteMe = (WikiR wp, [("mode", "d")])
      defaultLayout $ do
        setTitle $ preEscapedText path
        $(widgetFile "wiki")
        addStylesheet $ StaticR css_hk_kate_css
        $(whamletFile "templates/deleteWiki.hamlet")


postWikiR :: WikiPage -> Handler RepHtml
postWikiR wp = do
  _method <- lookupPostParam "_method"
  case _method of
    Just "preview" -> previewWiki
    Just "commit"  -> putWikiR wp
    Just "delete"  -> deleteWikiR wp
    _ -> invalidArgs ["The possible values of '_method' are preview,commit,delete."]
  where
    
    previewWiki :: Handler RepHtml
    previewWiki = do
      (Entity uid _) <- requireAuth
      msgShow <- getMessageRender
      let path = pathOf wp
          isTop = wp == topPage
      (raw, com, ver, donttouch) <- runInputPost $ (,,,)
                                    <$> ireq textField "content"
                                    <*> iopt textField "comment"
                                    <*> ireq intField "version"
                                    <*> iopt textField "donttouch"
      content <- runDB $ markdownToWikiHtml (wikiWriterOption msgShow) raw
      langs <- languages
      let editMe = (WikiR wp, [("mode", "e")])
          deleteMe = (WikiR wp, [("mode", "d")])
          markdown = getDoc langs
          toBool = maybe False (const True)
      defaultLayout $ do
        setTitle $ preEscapedText path
        $(widgetFile "wiki")
        addStylesheet $ StaticR css_hk_kate_css
        $(whamletFile "templates/previewWiki.hamlet")

putWikiR :: WikiPage -> Handler RepHtml
putWikiR wp = do
  (Entity uid _) <- requireAuth
  msgShow <- getMessageRender
  let path = pathOf wp
  now <- liftIO getCurrentTime
  (raw, com, ver, donttouch) <- runInputPost $ (,,,)
                                <$> ireq textField "content"
                                <*> iopt textField "comment"
                                <*> ireq intField "version"
                                <*> iopt textField "donttouch"
  runDB $ do
    (Entity pid page) <- getBy404 $ UniqueWiki path
    let oldtouched = wikiTouched page
        touched = maybe (Just now) (const oldtouched) donttouch
    if wikiVersion page == ver
      then do
      insert WikiHistory { wikiHistoryWiki=pid
                         , wikiHistoryPath=path
                         , wikiHistoryContent=raw
                         , wikiHistoryUpdated=now
                         , wikiHistoryTouched=touched
                         , wikiHistoryVersion=ver+1
                         , wikiHistoryEditor=uid
                         , wikiHistoryComment=com
                         }
      update pid [ WikiContent =. raw
                 , WikiUpdated =. now
                 , WikiTouched =. touched
                 , WikiVersion +=. 1
                 , WikiEditor =. uid
                 , WikiComment =. com]
      return pid
      else do
      -- FIXME Conflict?
      lift $ setMessage $ preEscapedText $ msgShow MsgConflictOccurred
      return pid
  redirect (WikiR wp, [("mode", "v")] :: [(Text, Text)])

deleteWikiR :: WikiPage -> Handler RepHtml
deleteWikiR wp = do
  _ <- requireAuth
  let path = pathOf wp
  runDB $ do
    (Entity pid _) <- getBy404 $ UniqueWiki path
    deleteWhere [WikiHistoryWiki ==. pid]
    delete pid
    return pid
  redirect (NewR, [("path", path),("mode", "v")] :: [(Text, Text)])

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
      msgShow <- getMessageRender
      path'' <- lookupGetParam "path"
      case path'' of
        Nothing -> invalidArgs ["'path' query paramerter is required."]
        Just path -> do
          let isTop = path==Settings.topTitle
              viewMe = (NewR, [("path", path), ("mode", "v")])
              editMe = (NewR, [("path", path), ("mode", "e")])
          defaultLayout $ do
            setTitle $ preEscapedText path
            $(widgetFile "wiki")
            addStylesheet $ StaticR css_hk_kate_css
            $(whamletFile "templates/viewNew.hamlet")
    
    editNew :: Handler RepHtml
    editNew = do
      (Entity uid _) <- requireAuth
      msgShow <- getMessageRender
      path'' <- lookupGetParam "path"
      case path'' of
        Nothing -> invalidArgs ["'path' query paramerter is required."]
        Just path -> do
          langs <- languages
          let isTop = path==Settings.topTitle
              viewMe = (NewR, [("path", path), ("mode", "v")])
              editMe = (NewR, [("path", path), ("mode", "e")])
              markdown = getDoc langs
              toBool = maybe False (const True)
              donttouch = if isSidePane path then Just undefined else Nothing
          defaultLayout $ do
            setTitle $ preEscapedText path
            $(widgetFile "wiki")
            addStylesheet $ StaticR css_hk_kate_css
            $(whamletFile "templates/editNew.hamlet")
  
postNewR :: Handler RepHtml
postNewR = do
  _ <- requireAuth
  _method <- lookupPostParam "_method"
  case _method of
    Just "preview" -> previewWiki
    Just "commit"  -> createWiki
    _              -> invalidArgs ["The possible values of '_method' are preview,commit."]
  where
    previewWiki :: Handler RepHtml
    previewWiki = do
      (Entity uid _) <- requireAuth
      msgShow <- getMessageRender
      (path, raw, com, donttouch) <- runInputPost $ (,,,)
                                      <$> ireq textField "path"
                                      <*> ireq textField "content"
                                      <*> iopt textField "comment"
                                      <*> iopt textField "donttouch"
      langs <- languages
      let isTop = path == Settings.topTitle
          viewMe = (NewR, [("path", path), ("mode", "v")])
          editMe = (NewR, [("path", path), ("mode", "e")])
          markdown = getDoc langs
          toBool = maybe False (const True)
      content <- runDB $ markdownToWikiHtml (wikiWriterOption msgShow) raw
      defaultLayout $ do
        setTitle $ preEscapedText path
        $(widgetFile "wiki")
        addStylesheet $ StaticR css_hk_kate_css
        $(whamletFile "templates/previewNew.hamlet")
    
    createWiki :: Handler RepHtml
    createWiki = do
      (Entity uid _) <- requireAuth
      (path, raw, com, donttouch) <- runInputPost $ (,,,)
                                     <$> ireq textField "path"
                                     <*> ireq textField "content"
                                     <*> iopt textField "comment"
                                     <*> iopt textField "donttouch"
      now <- liftIO getCurrentTime
      runDB $ do
        let touched = maybe (Just now) (const Nothing) donttouch
        pid <- insert Wiki { 
          wikiPath=path
        , wikiContent=raw
        , wikiUpdated=now
        , wikiTouched=touched
        , wikiVersion=0
        , wikiEditor=uid
        , wikiComment=com
        }
        insert WikiHistory {
          wikiHistoryWiki=pid
        , wikiHistoryPath=path
        , wikiHistoryContent=raw
        , wikiHistoryUpdated=now
        , wikiHistoryTouched=touched
        , wikiHistoryVersion=0
        , wikiHistoryEditor=uid
        , wikiHistoryComment=com
        }
        -- FIXME: use sendResponseCreated API
      redirect (WikiR $ fromPath path, [("mode", "v")] :: [(Text, Text)])

getHistoriesR :: WikiPage -> Handler RepHtml
getHistoriesR wp = do
  ver <- lookupGetParam "ver"
  case ver of
    Nothing -> historyList $ -1
    Just v  -> historyList $ read $ T.unpack v
  where
    -- Utility
    getHistories :: Handler [(User, WikiHistory)]
    getHistories = do
      let path = pathOf wp
      runDB $ do
        (Entity pid _) <- getBy404 $ UniqueWiki path
        hists' <- selectList [WikiHistoryWiki ==. pid] [Desc WikiHistoryVersion]
        hists <- forM hists' $ \(Entity _ h) -> do
          Just u <- get $ wikiHistoryEditor h
          return (u, h)
        return hists
    
    mkHistsWithDiff :: [(User, WikiHistory)] -> [(User, WikiHistory, (Version, Version))]
    mkHistsWithDiff hs = zipWith p2t hs diffs
      where 
        p2t :: (a, b) -> c -> (a, b, c)
        p2t (x, y) z = (x, y, z)
        new = map (T.lines . wikiHistoryContent . snd) hs
        old = tail new ++ [[]]
        diffs = zipWith ((foldr dc (0,0).).getDiff) new old
        dc (F,_) (f,s) = (f+1,s)
        dc (S,_) (f,s) = (f,s-1)
        dc _     fs    = fs
    
    -- pages
    historyList :: Version -> Handler RepHtml
    historyList ver = do
      mu <- maybeAuth
      msgShow <- getMessageRender
      let path = pathOf wp
          isTop = wp == topPage
          isNull = (""==)
          pagingSize = 25
      hs'' <- getHistories
      let hs' = mkHistsWithDiff hs''
          v' = if ver >= 0 then ver else curver
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
          altClass :: WikiHistory -> Text
          altClass = \h -> if wikiHistoryVersion h `mod` 2 == 0 then "even" else "odd"
          mnext = if v' >= pagingSize
                  then Just (HistoriesR wp, [("ver", T.pack . show $ v'-pagingSize)])
                  else Nothing
      defaultLayout $ do
        setTitle $ preEscapedText path
        $(widgetFile "wiki")
        $(whamletFile "templates/listHistories.hamlet")

        

getHistoryR :: Version -> WikiPage -> Handler RepHtml
getHistoryR vsn wp = do
  mode <- lookupGetParam "mode"
  case mode of
    Just "v" {-       view       -} -> viewHistory vsn
    Just "e" {-       edit       -} -> editHistory vsn
    Just "p" {- diff to previous -} -> diffPrevious vsn
    Just "c" {- diff to current  -} -> diffCurrent vsn
    Just "r" {-      revert      -} -> revertHistory vsn
    _        {-      illegal     -} -> invalidArgs ["The possible values of 'mode' are l,v,e,p,c,r."]
  where
    -- Utility
    getHistory :: Version -> Handler (Text, Text, Html, UTCTime, Version, Maybe User, Bool, Wiki)
    getHistory v = do
      msgShow <- getMessageRender
      let path = pathOf wp
      runDB $ do
        (Entity pid' p') <- getBy404 $ UniqueWiki path
        (Entity _ p) <- getBy404 $ UniqueWikiHistory pid' v
        me <- get $ wikiHistoryEditor p
        let (raw, upd, ver) = (wikiHistoryContent p, wikiHistoryUpdated p, wikiHistoryVersion p)
            isTop = wp == topPage
        content <- markdownToWikiHtml (wikiWriterOption msgShow) raw
        return (path, raw, content, upd, ver, me, isTop, p')

    getHistories :: Handler [(User, WikiHistory)]
    getHistories = do
      let path = pathOf wp
      runDB $ do
        (Entity pid _) <- getBy404 $ UniqueWiki path
        hists' <- selectList [WikiHistoryWiki ==. pid] [Desc WikiHistoryVersion]
        hists <- forM hists' $ \(Entity _ h) -> do
          Just u <- get $ wikiHistoryEditor h
          return (u, h)
        return hists
            
    mkDiff :: WikiHistory -> WikiHistory -> Html
    mkDiff new old = preEscapedText $ foldr d2h "" diffs
      where
        diffs = getDiff (lines' new) (lines' old)
        lines' = T.lines . wikiHistoryContent
        d2h (F, l) xs = "<span class='plus'>+&nbsp;" +++ l +++ "</span><br/>" +++ xs
        d2h (S, l) xs = "<span class='minus'>-&nbsp;" +++ l +++ "</span><br/>" +++ xs
        d2h (B, l) xs = "<span>&nbsp;&nbsp;" +++ l +++ "</span><br/>" +++ xs
        
    -- pages
    viewHistory :: Version -> Handler RepHtml
    viewHistory v = do
      msgShow <- getMessageRender
      (path, raw, content, upd, _, me, isTop, curp) <- getHistory v
      let editMe = (WikiR wp, [("mode", "e")])
          deleteMe = (WikiR wp, [("mode", "d")])
          ver = wikiVersion curp
          notCurrent =  v /= ver
          editVer = (HistoryR v wp, [("mode", "e")])
          currDiff = (HistoryR v wp, [("mode", "c")])
      defaultLayout $ do
        setTitle $ preEscapedText path
        $(widgetFile "wiki")
        addStylesheet $ StaticR css_hk_kate_css
        $(whamletFile "templates/viewHistory.hamlet")

    editHistory :: Version -> Handler RepHtml
    editHistory v = do
      (Entity uid _) <- requireAuth
      msgShow <- getMessageRender
      (path, raw, content, upd, _, me, isTop, curp) <- getHistory v
      langs <- languages
      let editMe = (WikiR wp, [("mode", "e")])
          deleteMe = (WikiR wp, [("mode", "d")])
          ver = wikiVersion curp
          notCurrent =  v /= ver
          markdown = getDoc langs
          toBool = maybe False (const True)
          donttouch = Just undefined
      defaultLayout $ do
        setTitle $ preEscapedText path
        $(widgetFile "wiki")
        addStylesheet $ StaticR css_hk_kate_css
        $(whamletFile "templates/editHistory.hamlet")
    
    revertHistory :: Version -> Handler RepHtml
    revertHistory v = do
      (Entity uid _) <- requireAuth
      msgShow <- getMessageRender
      (path, raw, content, upd, _, me, isTop, curp) <- getHistory v
      let editMe = (WikiR wp, [("mode", "e")])
          deleteMe = (WikiR wp, [("mode", "d")])
          ver = wikiVersion curp
          notCurrent =  v /= ver
          toBool = maybe False (const True)
          donttouch = Just undefined
      defaultLayout $ do
        setTitle $ preEscapedText path
        $(widgetFile "wiki")
        addStylesheet $ StaticR css_hk_kate_css
        $(whamletFile "templates/revertHistory.hamlet")

    diffVers :: (Wiki -> Version -> [Version]) -> Version -> Handler RepHtml
    diffVers selver v = do
      msgShow <- getMessageRender
      let path = pathOf wp
      (p, v1, v0) <- runDB $ do
        (Entity pid p) <- getBy404 $ UniqueWiki path
        [(Entity _ v1),(Entity _ v0)] <- 
          selectList [WikiHistoryWiki ==. pid, WikiHistoryVersion <-. (selver p v)]
                     [Desc WikiHistoryVersion, LimitTo 2]
        return (p, v1, v0)
      let editMe = (WikiR wp, [("mode", "e")])
          deleteMe = (WikiR wp, [("mode", "d")])
          title = if wikiVersion p == wikiHistoryVersion v1
                  then msgShow $ MsgChangesSince $ wikiHistoryUpdated v0
                  else msgShow $ MsgChangesBetween (wikiHistoryUpdated v0) (wikiHistoryUpdated v1)
          content = mkDiff v1 v0
          isTop = wp == topPage
      defaultLayout $ do
        setTitle $ preEscapedText path
        $(widgetFile "wiki")
        $(whamletFile "templates/diffHistories.hamlet")
    
    diffPrevious :: Version -> Handler RepHtml
    diffPrevious = diffVers $ \_ v -> [v, v-1]
    
    diffCurrent :: Version -> Handler RepHtml
    diffCurrent = diffVers $ \p v -> [wikiVersion p, v]


postHistoryR :: Version -> WikiPage -> Handler RepHtml
postHistoryR vsn wp = do
  _method <- lookupPostParam "_method"
  case _method of
    Just "preview" -> previewHistory
    Just "commit"  -> putWikiR wp
    Just "modify"  -> putHistoryR vsn wp
    _              -> invalidArgs ["The possible values of '_method' are preview,commit,modify"]
  where
    
    previewHistory :: Handler RepHtml
    previewHistory = do
      (Entity uid _) <- requireAuth
      msgShow <- getMessageRender
      let path = pathOf wp
          isTop = wp == topPage
      (raw, com, ver, v, donttouch) <- runInputPost $ (,,,,)
                            <$> ireq textField "content"
                            <*> iopt textField "comment"
                            <*> ireq intField "version"
                            <*> ireq intField "original_version"
                            <*> iopt textField "donttouch"
      content <- runDB $ markdownToWikiHtml (wikiWriterOption msgShow) raw
      langs <- languages
      let editMe = (WikiR wp, [("mode", "e")])
          deleteMe = (WikiR wp, [("mode", "d")])
          notCurrent = v /= ver
          markdown = getDoc langs
          toBool = maybe False (const True)
      defaultLayout $ do
        setTitle $ preEscapedText path
        $(widgetFile "wiki")
        addStylesheet $ StaticR css_hk_kate_css
        $(whamletFile "templates/previewHistory.hamlet")

putHistoryR :: Version -> WikiPage -> Handler RepHtml
putHistoryR v wp = do
  _ <- requireAuth
  let path = pathOf wp
  com <- runInputPost $ iopt textField "comment"
  runDB $ do
    (Entity pid _) <- getBy404 $ UniqueWiki path
    (Entity hid _) <- getBy404 $ UniqueWikiHistory pid v
    update hid [ WikiHistoryComment =. com ]
  hamletToRepHtml
    [hamlet|$newline never
$maybe c <- com
  <span>#{c}
$nothing
  <span>*** no log comment ***
|]
