{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Profile where

import Kestrel
import Settings (hamletFile, juliusFile)

import Control.Monad (unless)

getProfileR :: UserId -> Handler RepHtml
getProfileR uid = do
  (uid', _) <- requireAuth
  unless (uid' == uid) $ do
    permissionDenied "他人のプロフィールは見ることができません."
  u <- runDB $ get404 uid
  defaultLayout $ do
    setTitle "Profile"
    addJulius $(juliusFile "profile")
    addHamlet $(hamletFile "viewProfile")

postProfileR :: UserId -> Handler ()
postProfileR uid = do
  _method <- lookupPostParam "_method"
  case _method of
    Just "modify" -> putProfileR uid
    _             ->invalidArgs ["The possible values of '_method' is modify"]

putProfileR :: UserId -> Handler ()
putProfileR uid = do
  (uid', _) <- requireAuth
  unless (uid' == uid) $ do
    permissionDenied "You couldn't access another user profile."
  nn <- runFormPost' $ stringInput "nickname"
  runDB $ update uid [UserNickname $ Just nn]
  setMessage "プロフィールを更新しました."
  redirect RedirectTemporary $ ProfileR uid
