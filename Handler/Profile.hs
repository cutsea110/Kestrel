module Handler.Profile where

import Import hiding (head)
import Text.Julius (juliusFile)

import Yesod.Goodies.PNotify

getProfileR :: UserId -> Handler Html
getProfileR uid = do
  (Entity uid' _) <- requireAuth
  msgShow <- getMessageRender
  unless (uid' == uid) $ do
    permissionDenied $ msgShow MsgCouldntAccessAnotherUserProfile
  u <- runDB $ get404 uid
  defaultLayout $ do
    setTitle "Profile"
    toWidget $(juliusFile "templates/profile.julius")
    $(whamletFile "templates/viewProfile.hamlet")

postProfileR :: UserId -> Handler ()
postProfileR uid = do
  _method <- lookupPostParam "_method"
  case _method of
    Just "modify" -> putProfileR uid
    _             ->invalidArgs ["The possible values of '_method' is modify"]

putProfileR :: UserId -> Handler ()
putProfileR uid = do
  (Entity uid' _) <- requireAuth
  msgShow <- getMessageRender
  unless (uid' == uid) $ do
    permissionDenied $ msgShow MsgCouldntAccessAnotherUserProfile
  nn <- runInputPost $ ireq textField "nickname"
  runDB $ update uid [UserNickname =. Just nn]
  setPNotify $ PNotify JqueryUI Success "Updated" $ msgShow MsgUpdatedProfile
  redirect $ ProfileR uid
