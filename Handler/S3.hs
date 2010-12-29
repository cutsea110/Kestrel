{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.S3 where

import Kestrel
import Data.Time
import qualified Data.ByteString.Lazy as L
import Network.Wai
import Data.List (intercalate)
import System.Directory
import System.FilePath
import Web.Encodings (encodeUrl, decodeUrl)

getUploadR :: Handler RepHtml
getUploadR = do
  (uid,_) <- requireAuth
  defaultLayout $ do
    addCassius $(cassiusFile "s3/s3")
    addWidget $(widgetFile "s3/upload")

postUploadR :: Handler RepHtml
postUploadR = do
  (uid@(UserId uid'), _) <- requireAuth
  mfi <- lookupFile "upfile"
  case mfi of
    Nothing -> invalidArgs ["upload file is required"]
    Just fi -> do
      now <- liftIO getCurrentTime
      let (name, ext) = splitExtension $ fileName fi
          efname = encodeUrl $ fileName fi
      fid@(FileHeaderId fid') <- 
        runDB $ insert FileHeader {
            fileHeaderFullname=fileName fi
          , fileHeaderEfname=efname
          , fileHeaderContentType=fileContentType fi
          , fileHeaderName=name
          , fileHeaderExtension=ext
          , fileHeaderCreator=uid
          , fileHeaderCreated=now
          }
      let s3dir = "s3" </> show uid'
          s3fp = s3dir </> show fid'
      liftIO $ do
        createDirectoryIfMissing True s3dir
        L.writeFile s3fp (fileContent fi)
      redirect RedirectSeeOther $ FileR uid fid


getFileR :: UserId -> FileHeaderId -> Handler RepHtml
getFileR uid@(UserId uid') fid@(FileHeaderId fid') = do
  h <- runDB $ get404 fid
  let UserId id = fileHeaderCreator h
      s3dir = "s3" </> show uid'
      s3fp = s3dir </> show fid'
  b <- liftIO $ L.readFile s3fp
  setHeader "Content-Type" $ fileHeaderContentType h
  return $ RepHtml $ ResponseLBS b

postFileR :: UserId -> FileHeaderId -> Handler RepHtml
postFileR uid@(UserId uid') fid@(FileHeaderId fid') = undefined

    
getFileListR :: UserId -> Handler RepJson
getFileListR uid@(UserId uid') = do
  render <- getUrlRender
  files <- runDB $ selectList [FileHeaderCreatorEq uid] [FileHeaderCreatedDesc] 0 0
  cacheSeconds 3600
  jsonToRepJson $ jsonMap [("files", jsonList $ map (go render) files)]
  where
    go r (fid@(FileHeaderId fid'), f@FileHeader
               { fileHeaderFullname = name
               , fileHeaderExtension = ext
               , fileHeaderCreated = cdate
               }) = 
      jsonMap [ ("name", jsonScalar name)
              , ("ext" , jsonScalar ext)
              , ("cdate", jsonScalar $ show cdate)
              , ("url", jsonScalar $ r $ FileR uid fid)
              ]
