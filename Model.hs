{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module Model where

import Yesod
import Yesod.Helpers.Crud
import Database.Persist.TH (share2)
import Database.Persist.GenericSql (mkMigrate)
import Data.Time
import Data.Int
import Data.Maybe (fromMaybe)
import System.Locale

import qualified Settings (tz)

type Version = Int

-- You can define all of your database entities here. You can find more
-- information on persistent and how to declare entities at:
-- http://docs.yesodweb.com/book/persistent/
share2 mkPersist (mkMigrate "migrateAll") [$persist|
User
    ident String Asc
    password String Maybe Update
    nickname String Maybe Update
    active Bool Eq default=true
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

instance Item User where
  itemTitle = userInfoOneline

userDisplayName :: User -> String
userDisplayName (User _ _ (Just x) _) = x
userDisplayName (User x _ _ _) = x

showDate :: UTCTime -> String
showDate = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" . utc2local
  where
    utc2local = utcToLocalTime $ hoursToTimeZone Settings.tz

userInfoOneline :: User -> String
userInfoOneline u = "[" ++ showPrettyActive u ++ "] " ++ userIdent u ++ " (" ++ nickname u ++ ")"
  where
    nickname = fromMaybe "no nickname" . userNickname

showPrettyActive :: User -> String
showPrettyActive u = if userActive u then "有効" else "無効"
