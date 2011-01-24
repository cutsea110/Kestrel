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
import Data.Int
import qualified Data.ByteString.Lazy as L
import Network.Wai
import Data.List (intercalate)
import System.Directory
import System.FilePath
import Web.Encodings (encodeUrl, decodeUrl)

import qualified Settings (approot, rootRelativePath, s3dir, s3root)
import Settings (widgetFile, cassiusFile)

getUploadR :: Handler RepHtml
getUploadR = do
  (uid,_) <- requireAuth
  defaultLayout $ do
    addCassius $(cassiusFile "s3/s3")
    addWidget $(widgetFile "s3/upload")

upload :: UserId -> FileInfo -> Handler (FileHeaderId, String, String, Int64, UTCTime)
upload uid@(UserId uid') fi = do
  now <- liftIO getCurrentTime
  let (name, ext) = splitExtension $ fileName fi
      efname = encodeUrl $ fileName fi
      fsize = L.length $ fileContent fi
  fid@(FileHeaderId fid') <- 
    runDB $ insert FileHeader {
        fileHeaderFullname=fileName fi
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
  return (fid, fileName fi, ext, fsize, now)

postUploadR :: Handler RepXml
postUploadR = do
  (uid, _) <- requireAuth
  mfi <- lookupFile "upfile"
  case mfi of
    Nothing -> invalidArgs ["upload file is required."]
    Just fi -> do
      r <- getUrlRender
      (fid@(FileHeaderId f), name, ext, fsize, cdate) <- upload uid fi
      cacheSeconds 10 -- FIXME
      let rf = dropPrefix Settings.rootRelativePath $ r $ FileR uid fid
      fmap RepXml $ hamletToContent
#if GHC7
                      [xhamlet|
#else
                      [$xhamlet|
#endif
%file
  %name $name$
  %ext $ext$
  %size $show.fsize$
  %cdate $show.cdate$
  %uri $rf$
|]

putUploadR :: Handler RepHtml
putUploadR = do
  (uid, _) <- requireAuth
  mfi <- lookupFile "upfile"
  case mfi of
    Nothing -> invalidArgs ["upload file is required."]
    Just fi -> upload uid fi >>= 
               \(fid, _, _, _, _) -> sendResponseCreated $ FileR uid fid


getFileR :: UserId -> FileHeaderId -> Handler RepHtml
getFileR uid@(UserId uid') fid@(FileHeaderId fid') = do
  h <- runDB $ get404 fid
  let UserId id = fileHeaderCreator h
      s3dir = Settings.s3dir </> show uid'
      s3fp = s3dir </> show fid'
  b <- liftIO $ L.readFile s3fp
  setHeader "Content-Type" $ fileHeaderContentType h
  setHeader "Content-Disposition" $ "attachment; filename=" ++ fileHeaderEfname h
  return $ RepHtml $ ResponseLBS b

postFileR :: UserId -> FileHeaderId -> Handler RepXml
postFileR uid@(UserId uid') fid@(FileHeaderId fid') = do
  (uid, _) <- requireAuth
  _method <- lookupPostParam "_method"
  case _method of
    Just "delete" -> deleteFileR uid fid
    Nothing       -> invalidArgs ["The possible values of '_method' are delete."]

deleteFileR :: UserId -> FileHeaderId -> Handler RepXml
deleteFileR uid@(UserId uid') fid@(FileHeaderId fid') = do
  (uid'', _) <- requireAuth
  if uid/=uid''
    then
    invalidArgs ["You couldn't delete this resource."]
    else do
    r <- getUrlRender
    runDB $ delete fid
    let s3dir = Settings.s3dir </> show uid'
        s3fp = s3dir </> show fid'
        rf = dropPrefix Settings.rootRelativePath $ r $ FileR uid fid
    liftIO $ removeFile s3fp
    fmap RepXml $ hamletToContent
#if GHC7
                  [xhamlet|
#else
                  [$xhamlet|
#endif
%deleted
  %uri $rf$
|]

getFileListR :: UserId -> Handler RepJson
getFileListR uid@(UserId uid') = do
  render <- getUrlRender
  files <- runDB $ selectList [FileHeaderCreatorEq uid] [FileHeaderCreatedDesc] 0 0
  cacheSeconds 10 -- FIXME
  jsonToRepJson $ jsonMap [("files", jsonList $ map (go render) files)]
  where
    go r (fid@(FileHeaderId fid'), f@FileHeader
               { fileHeaderFullname = name
               , fileHeaderExtension = ext
               , fileHeaderFileSize = size
               , fileHeaderCreated = cdate
               }) = 
      jsonMap [ ("name", jsonScalar name)
              , ("ext" , jsonScalar ext)
              , ("size", jsonScalar $ show size)
              , ("cdate", jsonScalar $ show cdate)
              , ("uri", jsonScalar $ dropPrefix Settings.rootRelativePath $ r $ FileR uid fid)
              ]
