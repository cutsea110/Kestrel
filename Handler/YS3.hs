{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.YS3 where

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
    addCassius $(cassiusFile "ys3/ys3")
    addWidget $(widgetFile "ys3/upload")

postUploadR :: Handler RepHtml
postUploadR = do
  (uid, _) <- requireAuth
  mfi <- lookupFile "upfile"
  case mfi of
    Nothing -> invalidArgs ["upload file is required"]
    Just fi -> do
      now <- liftIO getCurrentTime
      let (name, ext) = splitExtension $ fileName fi
          efname = encodeUrl $ fileName fi
          UserId id = uid
          s3dir = "s3" </> show id
          s3fp = s3dir </> efname
      liftIO $ do
        createDirectoryIfMissing True s3dir
        L.writeFile s3fp (fileContent fi)
      fid <- runDB $ do
        insert FileHeader {
            fileHeaderFullname=fileName fi
          , fileHeaderEfname=efname
          , fileHeaderContentType=fileContentType fi
          , fileHeaderName=name
          , fileHeaderExtension=ext
          , fileHeaderCreator=uid
          , fileHeaderCreated=now
          }
      redirect RedirectSeeOther $ FileR uid fid


handleFileR :: UserId -> FileHeaderId -> Handler RepHtml
handleFileR uid fid = do
  h <- runDB $ get404 fid
  let UserId id = fileHeaderCreator h
      s3dir = "s3" </> show id
      s3fp = s3dir </> fileHeaderEfname h
  b <- liftIO $ L.readFile s3fp
  setHeader "Content-Type" $ fileHeaderContentType h
  return $ RepHtml $ ResponseLBS b

getFileListR :: UserId -> Handler RepHtml
getFileListR = undefined
