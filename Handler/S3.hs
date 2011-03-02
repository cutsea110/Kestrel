{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, CPP #-}
module Handler.S3 
       ( getUploadR
       , postUploadR
       , putUploadR
       , getFileR
       , postFileR
       , deleteFileR
       , getFileListR
       ) where

import Kestrel
import Data.Time
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Char8 (pack)
import System.Directory
import System.FilePath
import Web.Encodings (encodeUrl)

import qualified Settings (s3dir, rootbase)
import Settings (widgetFile, cassiusFile)

getUploadR :: Handler RepHtml
getUploadR = do
  (uid,_) <- requireAuth
  defaultLayout $ do
    addCassius $(cassiusFile "s3/s3")
    addWidget $(widgetFile "s3/upload")

-- | upload
--   :: (PersistBackend m, Control.Monad.IO.Class.MonadIO m) =>
--      Key User -> FileInfo -> m (Maybe (Key FileHeader, String, String, Int64, UTCTime))
upload uid@(UserId uid') fi = do
  if fileName fi /= "" && L.length (fileContent fi) > 0
    then do
    now <- liftIO getCurrentTime
    let (name, ext) = splitExtension $ fileName fi
        efname = encodeUrl $ fileName fi
        fsize = L.length $ fileContent fi
    fid@(FileHeaderId fid') <- 
      insert FileHeader { fileHeaderFullname=fileName fi
                        , fileHeaderEfname=efname
                        , fileHeaderContentType=fileContentType fi
                        , fileHeaderFileSize=fsize
                        , fileHeaderName=name
                        , fileHeaderExtension=ext
                        , fileHeaderCreator=uid
                        , fileHeaderCreated=now
                        }
    let s3dir = Settings.s3dir </> show uid'
        s3fp = s3dir </> show fid'
    liftIO $ do
      createDirectoryIfMissing True s3dir
      L.writeFile s3fp (fileContent fi)
    return $ Just (fid, fileName fi, ext, fsize, now)
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
        Just (fid, name, ext, fsize, cdate) -> do
          cacheSeconds 10 -- FIXME
          let rf = Settings.rootbase ++ (dropPrefix (approot y) $ r $ FileR uid fid)
          fmap RepXml $ hamletToContent
                      [$xhamlet|\
<file>
  <name>#{name}
  <ext>#{ext}
  <size>#{show fsize}
  <cdate>#{show cdate}
  <uri>#{rf}
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
        Just (fid, _, _, _, _) -> sendResponseCreated $ FileR uid fid


getFileR :: UserId -> FileHeaderId -> Handler RepHtml
getFileR (UserId uid') fid@(FileHeaderId fid') = do
  h <- runDB $ get404 fid
  let s3dir = Settings.s3dir </> show uid'
      s3fp = s3dir </> show fid'
  setHeader "Content-Type" $ pack $ fileHeaderContentType h
  setHeader "Content-Disposition" $ pack $ "attachment; filename=" ++ fileHeaderEfname h
  return $ RepHtml $ ContentFile s3fp

postFileR :: UserId -> FileHeaderId -> Handler RepXml
postFileR uid fid = do
  _ <- requireAuth
  _method <- lookupPostParam "_method"
  case _method of
    Just "delete" -> deleteFileR uid fid
    _ -> invalidArgs ["The possible values of '_method' are delete."]

deleteFileR :: UserId -> FileHeaderId -> Handler RepXml
deleteFileR uid@(UserId uid') fid@(FileHeaderId fid') = do
  (uid'', _) <- requireAuth
  y <- getYesod
  if uid/=uid''
    then
    invalidArgs ["You couldn't delete this resource."]
    else do
    r <- getUrlRender
    runDB $ delete fid
    let s3dir = Settings.s3dir </> show uid'
        s3fp = s3dir </> show fid'
        rf = Settings.rootbase ++ (dropPrefix (approot y) $ r $ FileR uid fid)
    liftIO $ removeFile s3fp
    fmap RepXml $ hamletToContent
                  [$xhamlet|\
<deleted>
  <uri>#{rf}
|]

getFileListR :: UserId -> Handler RepJson
getFileListR uid = do
  _ <- requireAuth
  y <- getYesod
  render <- getUrlRender
  files <- runDB $ selectList [FileHeaderCreatorEq uid] [FileHeaderCreatedDesc] 0 0
  cacheSeconds 10 -- FIXME
  jsonToRepJson $ jsonMap [("files", jsonList $ map (go y render) files)]
  where
    go y r (fid, FileHeader
               { fileHeaderFullname = name
               , fileHeaderExtension = ext
               , fileHeaderFileSize = size
               , fileHeaderCreated = cdate
               }) = 
      jsonMap [ ("name", jsonScalar name)
              , ("ext" , jsonScalar ext)
              , ("size", jsonScalar $ show size)
              , ("cdate", jsonScalar $ show cdate)
              , ("uri", jsonScalar $ Settings.rootbase ++ (dropPrefix (approot y) $ r $ FileR uid fid))
              ]
