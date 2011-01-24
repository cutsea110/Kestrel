{-# LANGUAGE QuasiQuotes, TypeFamilies #-}
{-# LANGUAGE CPP #-}
module Kestrel.Helpers.Auth.Account
    ( authAccount
    , YesodAuthAccount (..)
    , AccountCreds (..)
    , saltPass
    , loginR
    , setpassR
    ) where

import Yesod
import Network.Mail.Mime (randomString)
import Yesod.Helpers.Auth
import System.Random
import Control.Monad (when)
import Control.Applicative ((<$>), (<*>))
import Data.Digest.Pure.MD5
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (encodeUtf8)

loginR, setpassR :: AuthRoute
loginR = PluginR "account" ["login"]
setpassR = PluginR "account" ["set-password"]

type Account = String
type SaltedPass = String

-- | Data stored in a database for each user account.
data AccountCreds m = AccountCreds
    { accountCredsId :: AuthAccountId m
    , accountCredsAuthId :: Maybe (AuthId m)
    }

class YesodAuth m => YesodAuthAccount m where
    type AuthAccountId m

    showAuthAccountId :: m -> AuthAccountId m -> String
    readAuthAccountId :: m -> String -> Maybe (AuthAccountId m)

    getPassword :: AuthId m -> GHandler Auth m (Maybe SaltedPass)
    setPassword :: AuthId m -> SaltedPass -> GHandler Auth m ()
    getAccountCreds :: Account -> GHandler Auth m (Maybe (AccountCreds m))
    getAccount :: AuthAccountId m -> GHandler Auth m (Maybe Account)

    -- | Generate a random alphanumeric string.
    randomKey :: m -> IO String
    randomKey _ = do
        stdgen <- newStdGen
        return $ fst $ randomString 10 stdgen

authAccount :: YesodAuthAccount m => AuthPlugin m
authAccount =
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

postLoginR :: YesodAuthAccount master => GHandler Auth master ()
postLoginR = do
    (account, pass) <- runFormPost' $ (,)
        <$> stringInput "account"
        <*> stringInput "password"
    macreds <- getAccountCreds account
    maid <-
        case (macreds >>= accountCredsAuthId) of
            (Just aid) -> do
                mrealpass <- getPassword aid
                case mrealpass of
                    Nothing -> return Nothing
                    Just realpass -> return $
                        if isValidPass pass realpass
                            then Just aid
                            else Nothing
            _ -> return Nothing
    case maid of
        Just _aid -> do
            setCreds False $ Creds "account" account [] -- FIXME aid?
            y <- getYesod
            setMessage $ string "You are now Logged in"
            redirectUltDest RedirectTemporary $ loginDest y
        Nothing -> do
            setMessage $ string "Invalid account/password combination"
            toMaster <- getRouteToMaster
            redirect RedirectTemporary $ toMaster LoginR

getPasswordR :: YesodAuthAccount master => GHandler Auth master RepHtml
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

postPasswordR :: YesodAuthAccount master => GHandler Auth master ()
postPasswordR = do
    (new, confirm) <- runFormPost' $ (,)
        <$> stringInput "new"
        <*> stringInput "confirm"
    toMaster <- getRouteToMaster
    when (new /= confirm) $ do
        setMessage $ string "Passwords did not match, please try again"
        redirect RedirectTemporary $ toMaster setpassR
    maid <- maybeAuthId
    aid <- case maid of
            Nothing -> do
                setMessage $ string "You must be logged in to set a password"
                redirect RedirectTemporary $ toMaster loginR
            Just aid -> return aid
    salted <- liftIO $ saltPass new
    setPassword aid salted
    setMessage $ string "Password updated"
    y <- getYesod
    redirect RedirectTemporary $ loginDest y

saltLength :: Int
saltLength = 5

-- | Salt a password with a randomly generated salt.
saltPass :: String -> IO String
saltPass pass = do
    stdgen <- newStdGen
    let salt = take saltLength $ randomRs ('A', 'Z') stdgen
    return $ saltPass' salt pass

saltPass' :: String -> String -> String
saltPass' salt pass =
    salt ++ show (md5 $ fromString $ salt ++ pass)
  where
    fromString = encodeUtf8 . T.pack

isValidPass :: String -- ^ cleartext password
            -> SaltedPass -- ^ salted password
            -> Bool
isValidPass clear salted =
    let salt = take saltLength salted
     in salted == saltPass' salt clear
