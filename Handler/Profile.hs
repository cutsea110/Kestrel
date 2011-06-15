{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Profile where

import Kestrel
import Settings (juliusFile)

import Control.Monad (unless)
import Text.Hamlet (preEscapedText)

getProfileR :: UserId -> Handler RepHtml
getProfileR uid = do
  (uid', _) <- requireAuth
  msgShow <- getMessageRender
  unless (uid' == uid) $ do
    permissionDenied $ msgShow MsgCouldntAccessAnotherUserProfile
  u <- runDB $ get404 uid
  defaultLayout $ do
    setTitle "Profile"
    addJulius $(juliusFile "profile")
    addWidget $(whamletFile "hamlet/viewProfile.hamlet")

postProfileR :: UserId -> Handler ()
postProfileR uid = do
  _method <- lookupPostParam "_method"
  case _method of
    Just "modify" -> putProfileR uid
    _             ->invalidArgs ["The possible values of '_method' is modify"]

putProfileR :: UserId -> Handler ()
putProfileR uid = do
  (uid', _) <- requireAuth
  msgShow <- getMessageRender
  unless (uid' == uid) $ do
    permissionDenied $ msgShow MsgCouldntAccessAnotherUserProfile
  nn <- runFormPost' $ stringInput "nickname"
  runDB $ update uid [UserNickname $ Just nn]
  setMessage $ preEscapedText $ msgShow MsgUpdatedProfile
  redirect RedirectTemporary $ ProfileR uid
