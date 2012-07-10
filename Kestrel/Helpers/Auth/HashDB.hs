{-# LANGUAGE QuasiQuotes, TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
-- | replace Yesod.Helpers.Auth.HashDB
-- initialize password and insert user account manually.
-- How to make SHA1 password by
-- echo -n 'My Password' | sha1sum
--
module Kestrel.Helpers.Auth.HashDB
    ( authHashDB
    , YesodAuthHashDB (..)
    , HashDBCreds (..)
    , encrypt
    , loginR
    , setpassR
    ) where

import Yesod
import Yesod.Auth
import Control.Monad (unless)
import Control.Applicative ((<$>), (<*>))
import Data.ByteString.Lazy.Char8  (pack)
import Data.Digest.Pure.SHA        (sha1, showDigest)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Blaze.Internal (preEscapedText)

(+++) :: Text -> Text -> Text
(+++) = T.append

-- for I18N
data Message = AccountID
             | Password
             | IfYouWantToChangePassword
             | ChangePassword
             | NewPassword
             | Confirm
             | Update
             | LoginViaAccount
             | UpdatedPassword
             | PasswordDoesntMatch
             | ReEnterAgain

english :: Message -> Text
english AccountID = "Account ID"
english Password = "Password"
english IfYouWantToChangePassword = "If you want to change password,please login."
english ChangePassword = "Change Password"
english NewPassword = "New Password"
english Confirm = "Confirm"
english Update = "update"
english LoginViaAccount = "Login via account"
english UpdatedPassword = "your password updated."
english PasswordDoesntMatch = "The password doesn't match"
english ReEnterAgain = "Please re-enter again."

japanese :: Message -> Text
japanese AccountID = "アカウントID"
japanese Password = "パスワード"
japanese IfYouWantToChangePassword = "パスワードを変更するにはログインしてください."
japanese ChangePassword = "パスワード変更"
japanese NewPassword = "新パスワード"
japanese Confirm = "確認"
japanese Update = "変更"
japanese LoginViaAccount = "このアカウントでログインする"
japanese UpdatedPassword = "パスワードを更新しました."
japanese PasswordDoesntMatch = "パスワードが合致していません."
japanese ReEnterAgain = "再度入力しなおしてください."

type Language = Text
translate :: [Language] -> Message -> Text
translate ("en":_) = english
translate ("ja":_) = japanese
translate (_:rest) = translate rest
translate [] = english -- The default backup

instance RenderMessage master Message where
  renderMessage _ = translate

loginR, setpassR :: AuthRoute
loginR = PluginR "account" ["login"]
setpassR = PluginR "account" ["set-password"]

type HashDB = Text
type EncriptedPass = Text

-- | Data stored in a database for each user account.
data HashDBCreds m = HashDBCreds
    { hashdbCredsId :: AuthHashDBId m
    , hashdbCredsAuthId :: Maybe (AuthId m)
    }

class YesodAuth m => YesodAuthHashDB m where
    type AuthHashDBId m

    getPassword :: AuthId m -> GHandler Auth m (Maybe EncriptedPass)
    setPassword :: AuthId m -> EncriptedPass -> GHandler Auth m ()
    getHashDBCreds :: HashDB -> GHandler Auth m (Maybe (HashDBCreds m))
    getHashDB :: AuthHashDBId m -> GHandler Auth m (Maybe HashDB)

authHashDB :: YesodAuthHashDB m => AuthPlugin m
authHashDB =
    AuthPlugin "account" dispatch $ \tm ->
        [whamlet|\
<form method="post" action="@{tm loginR}">
    <table>
        <tr>
            <th>_{AccountID}
            <td>
                <input type="account" name="account">
        <tr>
            <th>_{Password}
            <td>
                <input type="password" name="password">
        <tr>
            <td colspan="2">
                <input type="submit" value="_{LoginViaAccount}">
|]
  where
    dispatch "POST" ["login"] = postLoginR >>= sendResponse
    dispatch "GET" ["set-password"] = getPasswordR >>= sendResponse
    dispatch "POST" ["set-password"] = postPasswordR >>= sendResponse
    dispatch _ _ = notFound

postLoginR :: YesodAuthHashDB master => GHandler Auth master ()
postLoginR = do
    (account, pass) <- runInputPost $ (,)
        <$> ireq textField "account"
        <*> ireq textField "password"
    macreds <- getHashDBCreds account
    maid <-
        case (macreds >>= hashdbCredsAuthId) of
            (Just aid) -> do
                mrealpass <- getPassword aid
                case mrealpass of
                    Nothing -> return Nothing
                    Just realpass -> return $
                        if encrypt pass == realpass
                            then Just aid
                            else Nothing
            _ -> return Nothing
    case maid of
        Just _aid -> do
            setCreds False $ Creds "account" account [] -- FIXME aid?
            y <- getYesod
            redirectUltDest $ loginDest y
        Nothing -> do
            toMaster <- getRouteToMaster
            redirect $ toMaster LoginR

getPasswordR :: YesodAuthHashDB master => GHandler Auth master RepHtml
getPasswordR = do
    toMaster <- getRouteToMaster
    msgShow <- getMessageRender
    maid <- maybeAuthId
    case maid of
        Just _ -> return ()
        Nothing -> do
            setMessage $ preEscapedText $ msgShow IfYouWantToChangePassword
            redirect $ toMaster loginR
    defaultLayout $ do
        setTitle $ preEscapedText $ msgShow ChangePassword
        [whamlet|\
<h3>_{ChangePassword}
<form method="post" action="@{toMaster setpassR}">
    <table>
        <tr>
            <th>_{NewPassword}
            <td>
                <input type="password" name="new">
        <tr>
            <th>_{NewPassword}(_{Confirm})
            <td>
                <input type="password" name="confirm">
        <tr>
            <td colspan="2">
                <input type="submit" value="_{Update}">
|]

postPasswordR :: YesodAuthHashDB master => GHandler Auth master ()
postPasswordR = do
    msgShow <- getMessageRender
    (new, confirm) <- runInputPost $ (,)
        <$> ireq textField "new"
        <*> ireq textField "confirm"
    toMaster <- getRouteToMaster
    unless (new == confirm) $ do
        setMessage $ preEscapedText $ msgShow PasswordDoesntMatch +++ msgShow ReEnterAgain
        redirect $ toMaster setpassR
    maid <- maybeAuthId
    aid <- case maid of
            Nothing -> do
                setMessage $ preEscapedText $ msgShow IfYouWantToChangePassword
                redirect $ toMaster loginR
            Just aid -> return aid
    let sha1pass = encrypt new
    setPassword aid sha1pass
    setMessage $ preEscapedText $ msgShow UpdatedPassword
    y <- getYesod
    redirect $ loginDest y

encrypt :: Text -> Text
encrypt = T.pack . showDigest . sha1 . pack . T.unpack
