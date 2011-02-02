{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Profile where

import Kestrel
import Settings (topTitle, hamletFile, cassiusFile, juliusFile, widgetFile)

import Control.Monad (when)

getProfileR :: UserId -> Handler RepHtml
getProfileR uid = do
  (uid', _) <- requireAuth
  when (uid' /= uid) $ do
    permissionDenied "You couldn't access another user profile."
  u <- runDB $ get404 uid
  defaultLayout $ do
    setTitle $ string "Profile"
    addHamlet $(hamletFile "viewProfile")

postProfileR :: UserId -> Handler ()
postProfileR uid = do
  nn <- runFormPost' $ stringInput "nickname"
  runDB $ update uid [UserNickname $ Just nn]
  setMessage "Your profile updated."
  redirect RedirectTemporary $ ProfileR uid
