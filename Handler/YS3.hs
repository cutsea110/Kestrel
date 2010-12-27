{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.YS3 where

import Kestrel
import Data.Time
import qualified Data.ByteString.Lazy as L (toChunks)
import qualified Data.ByteString as B (concat)

getUploadR :: Handler RepHtml
getUploadR = do
  (uid,_) <- requireAuth
  defaultLayout $ do
    addCassius $(cassiusFile "ys3/ys3")
    addWidget $(widgetFile "ys3/upload")

postUploadR :: Handler RepHtml
postUploadR = do
  (uid,_) <- requireAuth
  mfi <- lookupFile "upfile"
  case mfi of
    Nothing -> invalidArgs ["upload file is required"]
    Just fi -> do
      now <- liftIO getCurrentTime
      liftIO $ (putStrLn . show  . B.concat . L.toChunks . fileContent) fi
      fid <- runDB $ do
        fid' <- insert FileHeader {
            fileHeaderFullname=fileName fi
          , fileHeaderContentType=fileContentType fi
          , fileHeaderName=""
          , fileHeaderExtension=""
          , fileHeaderCreator=uid
          , fileHeaderCreated=now
          }
        insert FileBody {
            fileBodyHeader=fid'
          , fileBodyContent=(B.concat . L.toChunks . fileContent) fi
          }
        return fid'
      redirect RedirectSeeOther $ FileListR uid

handleFileR :: UserId -> FileHeaderId -> Handler RepHtml
handleFileR = undefined

getFileListR :: UserId -> Handler RepHtml
getFileListR = undefined
