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
  when (uid' /= uid) $ do
    permissionDenied "You couldn't access another user profile."
  nn <- runFormPost' $ stringInput "nickname"
  runDB $ update uid [UserNickname $ Just nn]
  setMessage "Your profile updated."
  redirect RedirectTemporary $ ProfileR uid
