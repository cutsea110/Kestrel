{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Admin where

import Foundation

import Yesod
import Control.Applicative ((<$>),(<*>))

getUsersR :: Handler RepHtml
getUsersR = do
  _ <- requireAuth
  users <- runDB $ selectList [] [Asc UserIdent]
  defaultLayout $(whamletFile "templates/users.hamlet")

getUserR :: UserId -> Handler RepHtml
getUserR uid = do
  _ <- requireAuth
  user <- runDB $ get404 uid
  defaultLayout $(whamletFile "templates/user.hamlet")

postUserR :: UserId -> Handler ()
postUserR uid = do
  _ <- requireAuth
  new <- runInputPost $ User
         <$> ireq textField "ident"
         <*> iopt passwordField "password"
         <*> iopt textField "nickname"
         <*> ireq boolField "active"
  runDB $ replace uid new
  redirect $ UserR uid

getNewUserR :: Handler RepHtml
getNewUserR = do
  _ <- requireAuth
  defaultLayout $(whamletFile "templates/newUser.hamlet")

postNewUserR :: Handler ()
postNewUserR = do
  _ <- requireAuth
  new <- runInputPost $ User
         <$> ireq textField "ident"
         <*> iopt passwordField "password"
         <*> iopt textField "nickname"
         <*> ireq boolField "active"
  uid <- runDB $ insert new {userPassword = Nothing }
  redirect $ UserR uid

getDeleteUserR :: UserId -> Handler RepHtml
getDeleteUserR uid = do
  _ <- requireAuth
  user <- runDB $ get404 uid
  defaultLayout $(whamletFile "templates/deleteUser.hamlet")

postDeleteUserR :: UserId -> Handler ()
postDeleteUserR uid = do
  _ <- requireAuth
  runDB $ delete uid
  redirect UsersR
