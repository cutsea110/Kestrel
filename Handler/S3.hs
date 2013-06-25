{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, CPP #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Handler.S3 
       ( getUploadR
       , postUploadR
       , putUploadR
       , getFileR
       , postFileR
       , deleteFileR
       , getFileListR
       , getThumbnailR
       ) where

import Foundation
import Kestrel.Helpers.Util (encodeUrl, ToText(..))

import Yesod
import Control.Applicative ((<$>))
import Data.Time
import Data.Conduit (($$))
import Data.Conduit.List (consume)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as L
import System.Directory
import System.FilePath
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Settings (s3dir, s3ThumbnailDir)
import Text.Cassius (cassiusFile)
import Text.Hamlet.XML
import Text.XML
import Graphics.Thumbnail

getUploadR :: Handler Html
getUploadR = do
  (Entity uid _) <- requireAuth
  defaultLayout $ do
    toWidget $(cassiusFile "templates/s3/s3.cassius")
    $(widgetFile "s3/upload")

upload uid fi = do
  lbs <- lift $ fileContent fi
  let fsize = L.length lbs
  if fileName' fi /= "" && fsize > 0
    then do
    now <- liftIO getCurrentTime
    let (name, ext) = splitExtension $ T.unpack $ fileName' fi
        efname = encodeUrl $ fileName' fi
    (et, width, height, imagep) <- liftIO $ do
      et <- mkThumbnail lbs
      case et of
        Right t -> return (et, Just (fst (orgSZ t)), Just (snd (orgSZ t)), True)
        Left _  -> return (et, Nothing, Nothing, False)
    fid <-
      insert FileHeader { fileHeaderFullname=fileName' fi
                        , fileHeaderEfname=efname
                        , fileHeaderContentType=fileContentType fi
                        , fileHeaderFileSize=fsize
                        , fileHeaderWidth=width
                        , fileHeaderHeight=height
                        , fileHeaderThumbnail=imagep
                        , fileHeaderName=T.pack name
                        , fileHeaderExtension=T.pack ext
                        , fileHeaderCreator=uid
                        , fileHeaderCreated=now
                        }
    let s3dir' = Settings.s3dir </> T.unpack (toPathPiece uid)
        s3fp = s3dir' </> T.unpack (toPathPiece fid)
        thumbDir = Settings.s3ThumbnailDir </> T.unpack (toPathPiece uid)
        thumbfp = thumbDir </> T.unpack (toPathPiece fid)
    liftIO $ do
      createDirectoryIfMissing True s3dir'
      L.writeFile s3fp lbs
      -- follow thumbnail
      case et of
        Right t -> do createDirectoryIfMissing True thumbDir
                      saveFile t thumbfp
        Left _ -> return ()
    return $ Just (fid, fileName' fi, T.pack ext, fsize, now, imagep, width, height)
    else return Nothing
  where
    fileName' :: FileInfo -> T.Text
    fileName' = last . T.split (\c -> c=='/' || c=='\\') . fileName
    fileContent f = L.fromChunks <$> (fileSource f $$ consume)

postUploadR :: Handler RepXml
postUploadR = do
  (Entity uid _) <- requireAuth
  y <- getYesod
  let (ApprootMaster approot') = approot
  mfi <- lookupFile "upfile"
  case mfi of
    Nothing -> invalidArgs ["upload file is required."]
    Just fi -> do
      r <- getUrlRender
      mf <- runDB $ upload uid fi
      case mf of
        Nothing -> invalidArgs ["upload file is required."]
        Just (fid, name, ext, fsize, cdate, imgp, wd, ht) -> do
          cacheSeconds 10 -- FIXME
          let rf = (dropPrefix (approot' y) $ r $ FileR uid fid)
              trf = (dropPrefix (approot' y) $ r $ ThumbnailR uid fid) 
          return $ RepXml $ toContent $ renderText def $
            Document
            (Prologue [] Nothing [])
            (Element "file" Map.empty [xml|
<fhid>#{toPathPiece fid}
<name>#{name}
<ext>#{ext}
<size>#{toText fsize}
<cdate>#{toText cdate}
<uri>#{rf}
$if imgp
  <thumbnail_uri>#{trf}
$else
  <thumbnail_uri>
$maybe w <- wd
  <width>#{toText w}
$nothing
  <width>
$maybe h <- ht
  <height>#{toText h}
$nothing
  <height>
|]) []

putUploadR :: Handler Html
putUploadR = do
  (Entity uid _) <- requireAuth
  mfi <- lookupFile "upfile"
  case mfi of
    Nothing -> invalidArgs ["upload file is required."]
    Just fi -> do
      mf <- runDB $ upload uid fi
      case mf of
        Nothing -> invalidArgs ["upload file is required."]
        Just (fid, _, _, _, _, _, _, _) -> sendResponseCreated $ FileR uid fid


getFileR :: UserId -> FileHeaderId -> Handler ()
getFileR uid fid = do
  h <- runDB $ get404 fid
  let s3dir' = Settings.s3dir </> T.unpack (toPathPiece uid)
      s3fp = s3dir' </> T.unpack (toPathPiece fid)
  addHeader "Content-Type" $ fileHeaderContentType h
  addHeader "Content-Disposition" $ "attachment; filename=" +++ fileHeaderEfname h
  sendFile (TE.encodeUtf8 (fileHeaderContentType h)) s3fp

postFileR :: UserId -> FileHeaderId -> Handler RepXml
postFileR uid fid = do
  _ <- requireAuth
  _method <- lookupPostParam "_method"
  case _method of
    Just "delete" -> deleteFileR uid fid
    _ -> invalidArgs ["The possible values of '_method' are delete."]

deleteFileR :: UserId -> FileHeaderId -> Handler RepXml
deleteFileR uid fid = do
  (Entity uid'' _) <- requireAuth
  y <- getYesod
  let (ApprootMaster approot') = approot
  if uid/=uid''
    then
    invalidArgs ["You couldn't delete this resource."]
    else do
    r <- getUrlRender
    runDB $ delete fid
    let s3dir' = Settings.s3dir </> T.unpack (toPathPiece uid)
        s3fp = s3dir' </> T.unpack (toPathPiece fid)
        thumbDir = Settings.s3ThumbnailDir </> T.unpack (toPathPiece uid)
        thumbfp = thumbDir </> T.unpack (toPathPiece fid)
        rf = dropPrefix (approot' y) $ r $ FileR uid fid
    liftIO $ do
      exist <- doesFileExist s3fp
      if exist then removeFile s3fp else return ()
      exist' <- doesFileExist thumbfp
      if exist' then removeFile thumbfp else return ()
    return $ RepXml $ toContent $ renderText def $
      Document
      (Prologue [] Nothing [])
      (Element "deleted" Map.empty [xml|
<uri>#{rf}
|]) []

getFileListR :: UserId -> Handler Value
getFileListR uid = do
  _ <- requireAuth
  y <- getYesod
  render <- getUrlRender
  files <- runDB $ selectList [FileHeaderCreator ==. uid] [Desc FileHeaderCreated]
  cacheSeconds 10 -- FIXME
  returnJson $ object ["files" .= array (map (go y render) files)]
  where
    go y r (Entity fid FileHeader
               { fileHeaderFullname = name
               , fileHeaderExtension = ext
               , fileHeaderThumbnail = imgp
               , fileHeaderFileSize = size
               , fileHeaderWidth = width
               , fileHeaderHeight = height
               , fileHeaderCreated = cdate
               }) = 
      object [ "name" .= name
             , "ext" .= ext
             , "size" .= size
             , "cdate" .= cdate
             , "uri" .= (dropPrefix (approot' y) $ r $ FileR uid fid)
             , "thumbnail_uri" .= thumbnailUri
             , "width" .= w
             , "height" .= h
             ]
      where
        ApprootMaster approot' = approot
        w = maybe 0 id width
        h = maybe 0 id height
        thumbnailUri = if imgp
                       then dropPrefix (approot' y) $ r $ ThumbnailR uid fid
                       else ""

getThumbnailR :: UserId -> FileHeaderId -> Handler ()
getThumbnailR uid fid = do
  h <- runDB $ get404 fid
  let thumbDir = Settings.s3ThumbnailDir </> T.unpack (toPathPiece uid)
      thumbfp = thumbDir </> T.unpack (toPathPiece fid)
  addHeader "Content-Type" $ fileHeaderContentType h
  addHeader "Content-Disposition" $ "attachment; filename=" +++ fileHeaderEfname h
  sendFile (TE.encodeUtf8 (fileHeaderContentType h)) thumbfp
