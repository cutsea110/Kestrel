{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Kestrel.Helpers.Auth.Owl
       ( authOwl
       ) where

import Yesod hiding (object)
import Yesod.Auth

import Control.Applicative ((<$>),(<*>))
import Control.Monad (mzero)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Conduit as C
import Network.HTTP.Conduit

import Data.Aeson
import Data.Conduit.Binary (sourceLbs)
import Data.Conduit.Attoparsec (sinkParser)
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.HashMap.Strict as M (toList)
import qualified Yesod.Goodies.PNotify as P
import Kestrel.Helpers.Util
import Crypto.PubKey.RSA

-- for Request
data AuthReq = AuthReq
               { ident :: Text
               , pass :: Text
               }
             deriving (Show, Read, Eq)

instance FromJSON AuthReq where
  parseJSON (Object v) = AuthReq <$> v .: "ident" <*> v .: "pass"
  parseJSON _ = mzero
instance ToJSON AuthReq where
  toJSON (AuthReq i p) = object ["ident" .= i, "pass" .= p]

-- for Response
data OwlRes = OwlRes { cipher :: LB.ByteString }
instance FromJSON OwlRes where
  parseJSON (Object o) = OwlRes <$> o .: "cipher"
  parseJSON _ = mzero
instance ToJSON OwlRes where
  toJSON (OwlRes e) = object [ "cipher" .= e ]

data AuthRes = Rejected
               { rejected_ident :: Text
               , rejected_pass :: Text
               , rejected_reason :: Text
               }
             | Accepted
               { accepted_ident :: Text
               , accepted_email :: Maybe Text
               }
             deriving (Show, Read, Eq)

instance FromJSON AuthRes where
  parseJSON (Object o) = case M.toList o of
    [("rejected", Object o')] ->
      Rejected <$> o' .: "ident" <*> o' .: "pass" <*> o' .: "reason"
    [("accepted", Object o')] ->
      Accepted <$> o' .: "ident" <*> o' .:? "email"
  parseJSON _ = mzero

instance ToJSON AuthRes where
  toJSON (Rejected i p r) = object [ "rejected" .= object [ "ident" .= i
                                                          , "pass" .= p
                                                          , "reason" .= r
                                                          ]
                                   ]
  toJSON (Accepted i me) = object [ "accepted" .= object [ "ident" .= i
                                                         , "email" .= me
                                                         ]
                                  ]

type ServiceURL = String

authOwl :: YesodAuth m => SB.ByteString -> PublicKey -> PrivateKey -> ServiceURL -> AuthPlugin m
authOwl clientId owlPubkey myPrivkey ep =  AuthPlugin "owl" dispatch login
  where
    dispatch "POST" [] = do
      oreq <- getRequest
      (ident, pass) <- (,) <$> (runInputPost $ ireq textField "ident")
                           <*> (runInputPost $ ireq passwordField "password")
      req' <- lift $ parseUrl ep
      (e, _) <- liftIO $ encrypt owlPubkey $ encode $ AuthReq ident pass
      let req = req' { requestHeaders =
                          [ ("Content-Type", "application/json")
                          , ("X-Owl-clientId", clientId)
                          , ("X-Owl-signature", fromLazy $ sign myPrivkey e)
                          , ("Accept-Language", SB.pack $ T.unpack $ T.intercalate ";" $ reqLangs oreq)
                          ]
                     , method = "POST"
                     , requestBody = RequestBodyLBS e
                     }
      res <- http req =<< authHttpManager <$> getYesod
      v <- responseBody res $$+- sinkParser json
      case fromJSON v of
        Success (OwlRes e) -> do
          let plain = decrypt myPrivkey $ fromLazy e
          v' <- sourceLbs (toLazy plain) $$ sinkParser json
          case fromJSON v' of
            Success (Accepted i e) ->
              setCreds True $ Creds "owl" ident []
            Success (Rejected i p r) -> do
              P.setPNotify $ P.PNotify P.JqueryUI P.Error "login failed" r
              toMaster <- getRouteToMaster
              redirect $ toMaster LoginR
        Error msg -> invalidArgs [T.pack msg]
      case fromJSON v of
        Success (Accepted i e) ->
          setCreds True $ Creds "owl" ident []
        Success (Rejected i p r) -> do
          P.setPNotify $ P.PNotify P.JqueryUI P.Error "login failed" r
          toMaster <- getRouteToMaster
          redirect $ toMaster LoginR
        Error msg -> invalidArgs [T.pack msg]
    dispatch _ _ = notFound
    url = PluginR "owl" []
    login authToMaster =
      toWidget [hamlet|
<form method="post" action="@{authToMaster url}" .form-horizontal>
  <div .control-group.info>
    <label .control-label for=ident>Owl Account ID
    <div .controls>
      <input type=text #ident name=ident .span3 autofocus="" required>
  <div .control-group.info>
    <label .control-label for=ident>Owl Password
    <div .controls>
      <input type=password #password name=password .span3 required>
  <div .control-group>
    <div .controls.btn-group>
      <input type=submit .btn.btn-primary value=Login>
|]
