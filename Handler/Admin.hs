{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Admin where

import Foundation
import Kestrel.Helpers.Auth.HashDB (encrypt)

import Yesod
import Data.Text (Text)
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
  runDB $ do 
    orig <- get404 uid
    replace uid new { userPassword = pass orig new }
  redirect $ UserR uid
  where
    pass :: User -> User -> Maybe Text
    pass old new = maybe (userPassword old) (return . encrypt) (userPassword new)

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
  uid <- runDB $ insert new {userPassword = fmap encrypt (userPassword new)}
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
