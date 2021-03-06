module Handler.Admin where

import Import

getUsersR :: Handler Html
getUsersR = do
  _ <- requireAuth
  users <- runDB $ selectList [] [Asc UserIdent]
  defaultLayout $(whamletFile "templates/users.hamlet")

getUserR :: UserId -> Handler Html
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

getNewUserR :: Handler Html
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

getDeleteUserR :: UserId -> Handler Html
getDeleteUserR uid = do
  _ <- requireAuth
  user <- runDB $ get404 uid
  defaultLayout $(whamletFile "templates/deleteUser.hamlet")

postDeleteUserR :: UserId -> Handler ()
postDeleteUserR uid = do
  _ <- requireAuth
  runDB $ delete uid
  redirect UsersR
