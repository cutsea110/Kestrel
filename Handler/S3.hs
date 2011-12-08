{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, CPP #-}
{-# LANGUAGE GADTs #-}
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
import Data.Time
import Data.Int
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Char8 (pack)
import System.Directory
import System.FilePath
import Web.Encodings (encodeUrl)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Settings (s3dir, s3ThumbnailDir)
import Text.Cassius (cassiusFile)
import Graphics.Thumbnail

getUploadR :: Handler RepHtml
getUploadR = do
  (uid,_) <- requireAuth
  msg <- getMessageRender
  defaultLayout $ do
    addCassius $(cassiusFile "templates/s3/s3.cassius")
    addWidget $(widgetFile "s3/upload")

upload :: PersistBackend b m =>
          Key backend User -> FileInfo -> b m (Maybe (Key b (FileHeaderGeneric backend), Text, Text, Int64, UTCTime, Bool, Maybe Int, Maybe Int))
upload uid fi = do
  if fileName fi /= "" && L.length (fileContent fi) > 0
    then do
    now <- liftIO getCurrentTime
    let (name, ext) = splitExtension $ T.unpack $ fileName fi
        efname = encodeUrl $ fileName fi
        fsize = L.length $ fileContent fi
    (et, width, height, imagep) <- liftIO $ do
      et <- mkThumbnail (fileContent fi)
      case et of
        Right t -> return (et, Just (fst (orgSZ t)), Just (snd (orgSZ t)), True)
        Left _  -> return (et, Nothing, Nothing, False)
    fid <-
      insert FileHeader { fileHeaderFullname=fileName fi
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
    let s3dir' = Settings.s3dir </> show uid
        s3fp = s3dir' </> show fid
        thumbDir = Settings.s3ThumbnailDir </> show uid
        thumbfp = thumbDir </> show fid
    liftIO $ do
      createDirectoryIfMissing True s3dir'
      L.writeFile s3fp (fileContent fi)
      -- follow thumbnail
      case et of
        Right t -> do createDirectoryIfMissing True thumbDir
                      saveFile t thumbfp
        Left _ -> return ()
    return $ Just (fid, fileName fi, T.pack ext, fsize, now, imagep, width, height)
    else return Nothing

postUploadR :: Handler RepXml
postUploadR = do
  (uid, _) <- requireAuth
  y <- getYesod
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
          let rf = (dropPrefix (approot y) $ r $ FileR uid fid)
              trf = (dropPrefix (approot y) $ r $ ThumbnailR uid fid) 
          fmap RepXml $ hamletToContent
                      [xhamlet|\
<file>
  <fhid>#{show fid}
  <name>#{name}
  <ext>#{ext}
  <size>#{show fsize}
  <cdate>#{show cdate}
  <uri>#{rf}
  $if imgp
    <thumbnail_uri>#{trf}
  $else
    <thumbnail_uri>
  $maybe w <- wd
    <width>#{w}
  $nothing
    <width>
  $maybe h <- ht
    <height>#{h}
  $nothing
    <height>
|]

putUploadR :: Handler RepHtml
putUploadR = do
  (uid, _) <- requireAuth
  mfi <- lookupFile "upfile"
  case mfi of
    Nothing -> invalidArgs ["upload file is required."]
    Just fi -> do
      mf <- runDB $ upload uid fi
      case mf of
        Nothing -> invalidArgs ["upload file is required."]
        Just (fid, _, _, _, _, _, _, _) -> sendResponseCreated $ FileR uid fid


getFileR :: UserId -> FileHeaderId -> Handler RepHtml
getFileR uid fid = do
  h <- runDB $ get404 fid
  let s3dir' = Settings.s3dir </> show uid
      s3fp = s3dir' </> show fid
  setHeader "Content-Type" $ pack $ T.unpack $ fileHeaderContentType h
  setHeader "Content-Disposition" $ pack $ T.unpack $ "attachment; filename=" +++ fileHeaderEfname h
  return $ RepHtml $ ContentFile s3fp Nothing

postFileR :: UserId -> FileHeaderId -> Handler RepXml
postFileR uid fid = do
  _ <- requireAuth
  _method <- lookupPostParam "_method"
  case _method of
    Just "delete" -> deleteFileR uid fid
    _ -> invalidArgs ["The possible values of '_method' are delete."]

deleteFileR :: UserId -> FileHeaderId -> Handler RepXml
deleteFileR uid fid = do
  (uid'', _) <- requireAuth
  y <- getYesod
  if uid/=uid''
    then
    invalidArgs ["You couldn't delete this resource."]
    else do
    r <- getUrlRender
    runDB $ delete fid
    let s3dir' = Settings.s3dir </> show uid
        s3fp = s3dir' </> show fid
        thumbDir = Settings.s3ThumbnailDir </> show uid
        thumbfp = thumbDir </> show fid
        rf = (dropPrefix (approot y) $ r $ FileR uid fid)
    liftIO $ do
      exist <- doesFileExist s3fp
      if exist then removeFile s3fp else return ()
      exist' <- doesFileExist thumbfp
      if exist' then removeFile thumbfp else return ()
    fmap RepXml $ hamletToContent
                  [xhamlet|\
<deleted>
  <uri>#{rf}
|]

getFileListR :: UserId -> Handler RepJson
getFileListR uid = do
  _ <- requireAuth
  y <- getYesod
  render <- getUrlRender
  files <- runDB $ selectList [FileHeaderCreator ==. uid] [Desc FileHeaderCreated]
  cacheSeconds 10 -- FIXME
  jsonToRepJson $ jsonMap [("files", jsonList $ map (go y render) files)]
  where
    go y r (fid, FileHeader
               { fileHeaderFullname = name
               , fileHeaderExtension = ext
               , fileHeaderThumbnail = imgp
               , fileHeaderFileSize = size
               , fileHeaderWidth = width
               , fileHeaderHeight = height
               , fileHeaderCreated = cdate
               }) = 
      jsonMap [ ("name", jsonScalar $ T.unpack name)
              , ("ext" , jsonScalar $ T.unpack ext)
              , ("size", jsonScalar $ show size)
              , ("cdate", jsonScalar $ show cdate)
              , ("uri", jsonScalar $ T.unpack $ (dropPrefix (approot y) $ r $ FileR uid fid))
              , ("thumbnail_uri", thumbnailUri)
              , ("width", jsonScalar $ show w)
              , ("height", jsonScalar $ show h)
              ]
      where
        thumbnailUri = if imgp                                  
                       then jsonScalar $ T.unpack $ (dropPrefix (approot y) $ r $ ThumbnailR uid fid)
                       else jsonScalar ""
        w = case width of
          Just x -> x
          Nothing -> 0
        h = case height of
          Just x -> x
          Nothing -> 0

getThumbnailR :: UserId -> FileHeaderId -> Handler RepHtml
getThumbnailR uid fid = do
  h <- runDB $ get404 fid
  let thumbDir = Settings.s3ThumbnailDir </> show uid
      thumbfp = thumbDir </> show fid
  setHeader "Content-Type" $ pack $ T.unpack $ fileHeaderContentType h
  setHeader "Content-Disposition" $ pack $ T.unpack $ "attachment; filename=" +++ fileHeaderEfname h
  return $ RepHtml $ ContentFile thumbfp Nothing
