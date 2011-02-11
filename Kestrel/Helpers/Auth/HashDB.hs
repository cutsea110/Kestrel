{-# LANGUAGE QuasiQuotes, TypeFamilies #-}
{-# LANGUAGE CPP #-}
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
import Yesod.Helpers.Auth
import Control.Monad (unless)
import Control.Applicative ((<$>), (<*>))
import Data.ByteString.Lazy.Char8  (pack)
import Data.Digest.Pure.SHA        (sha1, showDigest)

loginR, setpassR :: AuthRoute
loginR = PluginR "account" ["login"]
setpassR = PluginR "account" ["set-password"]

type HashDB = String
type EncriptedPass = String

-- | Data stored in a database for each user account.
data HashDBCreds m = HashDBCreds
    { hashdbCredsId :: AuthHashDBId m
    , hashdbCredsAuthId :: Maybe (AuthId m)
    }

class YesodAuth m => YesodAuthHashDB m where
    type AuthHashDBId m

    showAuthHashDBId :: m -> AuthHashDBId m -> String
    readAuthHashDBId :: m -> String -> Maybe (AuthHashDBId m)

    getPassword :: AuthId m -> GHandler Auth m (Maybe EncriptedPass)
    setPassword :: AuthId m -> EncriptedPass -> GHandler Auth m ()
    getHashDBCreds :: HashDB -> GHandler Auth m (Maybe (HashDBCreds m))
    getHashDB :: AuthHashDBId m -> GHandler Auth m (Maybe HashDB)

authHashDB :: YesodAuthHashDB m => AuthPlugin m
authHashDB =
    AuthPlugin "account" dispatch $ \tm ->
#if GHC7
        [hamlet|
#else
        [$hamlet|
#endif
%form!method=post!action=@tm.loginR@
    %table
        %tr
            %th Account ID
            %td
                %input!type=account!name=account
        %tr
            %th Password
            %td
                %input!type=password!name=password
        %tr
            %td!colspan=2
                %input!type=submit!value="Login via account"
|]
  where
    dispatch "POST" ["login"] = postLoginR >>= sendResponse
    dispatch "GET" ["set-password"] = getPasswordR >>= sendResponse
    dispatch "POST" ["set-password"] = postPasswordR >>= sendResponse
    dispatch _ _ = notFound

postLoginR :: YesodAuthHashDB master => GHandler Auth master ()
postLoginR = do
    (account, pass) <- runFormPost' $ (,)
        <$> stringInput "account"
        <*> stringInput "password"
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
            redirectUltDest RedirectTemporary $ loginDest y
        Nothing -> do
            toMaster <- getRouteToMaster
            redirect RedirectTemporary $ toMaster LoginR

getPasswordR :: YesodAuthHashDB master => GHandler Auth master RepHtml
getPasswordR = do
    toMaster <- getRouteToMaster
    maid <- maybeAuthId
    case maid of
        Just _ -> return ()
        Nothing -> do
            setMessage $ string "You must be logged in to set a password"
            redirect RedirectTemporary $ toMaster loginR
    defaultLayout $ do
        setTitle $ string "Set password"
        addHamlet
#if GHC7
            [hamlet|
#else
            [$hamlet|
#endif
%h3 Set a new password
%form!method=post!action=@toMaster.setpassR@
    %table
        %tr
            %th New password
            %td
                %input!type=password!name=new
        %tr
            %th Confirm
            %td
                %input!type=password!name=confirm
        %tr
            %td!colspan=2
                %input!type=submit!value=Submit
|]

postPasswordR :: YesodAuthHashDB master => GHandler Auth master ()
postPasswordR = do
    (new, confirm) <- runFormPost' $ (,)
        <$> stringInput "new"
        <*> stringInput "confirm"
    toMaster <- getRouteToMaster
    unless (new == confirm) $ do
        setMessage $ string "Passwords did not match, please try again"
        redirect RedirectTemporary $ toMaster setpassR
    maid <- maybeAuthId
    aid <- case maid of
            Nothing -> do
                setMessage $ string "You must be logged in to set a password"
                redirect RedirectTemporary $ toMaster loginR
            Just aid -> return aid
    let sha1pass = encrypt new
    setPassword aid sha1pass
    setMessage $ string "Password updated"
    y <- getYesod
    redirect RedirectTemporary $ loginDest y

encrypt :: String -> String
encrypt = showDigest . sha1 . pack
