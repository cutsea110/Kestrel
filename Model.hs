{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Model where

import Yesod
-- import Yesod.Helpers.Crud
import Data.Time
import Data.Int
import Data.Maybe (fromMaybe)
import System.Locale
import Data.Monoid (mappend)
import Data.Text (Text)

import qualified Settings (tz)

type Version = Int

-- You can define all of your database entities here. You can find more
-- information on persistent and how to declare entities at:
-- http://docs.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"] $(persistFile "config/models")

-- FIXME Crud
-- instance Item User where
--  itemTitle = userInfoOneline

userDisplayName :: User -> Text
userDisplayName (User _ _ (Just x) _) = x
userDisplayName (User x _ _ _) = x

showDate :: UTCTime -> String
showDate = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" . utc2local
  where
    utc2local = utcToLocalTime $ hoursToTimeZone Settings.tz

userInfoOneline :: User -> Text
userInfoOneline u = "[" `mappend` showPrettyActive u `mappend` "] " `mappend` userIdent u `mappend` " (" `mappend` nickname u `mappend` ")"
  where
    nickname = fromMaybe "no nickname" . userNickname

showPrettyActive :: User -> Text
showPrettyActive u = if userActive u then "有効" else "無効"
