{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
module Model where

import Yesod
import Yesod.Form.Core
import Database.Persist.TH (share2)
import Database.Persist.GenericSql (mkMigrate)
import Data.Time
import Data.ByteString (ByteString)
import Data.Int

type Version = Int

-- You can define all of your database entities here. You can find more
-- information on persistent and how to declare entities at:
-- http://docs.yesodweb.com/book/persistent/
share2 mkPersist (mkMigrate "migrateAll") [$persist|
User
    ident String Asc
    password String Update toFormField=passwordField'
    UniqueUser ident

Email
    email String
    user UserId Maybe Update
    verkey String Maybe Update
    UniqueEmail email

Wiki
    path String Asc
    content String Update
    updated UTCTime Update Desc
    version Version default=0 Add
    editor UserId Update
    comment String Maybe Update
    UniqueWiki path

WikiHistory
    wiki WikiId Eq
    path String Asc Eq
    content String
    updated UTCTime Desc
    version Version Eq In Desc
    editor UserId
    comment String Maybe Update
    UniqueWikiHistory wiki version

FileHeader
    fullname String Eq
    efname String
    name String Eq
    extension String Eq
    contentType String
    fileSize Int64
    creator UserId Eq
    created UTCTime Desc
|]

passwordField' :: (IsForm f, FormType f ~ String)
              => FormFieldSettings -> Maybe String -> f
passwordField' = requiredFieldHelper passwordFieldProfile'
  where
    passwordFieldProfile' :: FieldProfile s m String
    passwordFieldProfile' = FieldProfile
      { fpParse = Right
      , fpRender = const ""
      , fpWidget = \theId name val isReq -> addHamlet
#if GHC7
[hamlet|
#else
[$hamlet|
#endif
%input#$theId$!name=$name$!type=password!:isReq:required!value=$val$
|]
      }

-- manually insert first User
-- account  : kestrel
-- password : kestrel
-- insert into "User" (ident,password) values ('kestrel','ZXQGB6393dfa5c4023381310fb84c864debf7');
