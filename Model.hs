{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving #-}
module Model where

import Yesod
import Database.Persist.TH (share2)
import Database.Persist.GenericSql (mkMigrate)
import Data.Time

-- You can define all of your database entities here. You can find more
-- information on persistent and how to declare entities at:
-- http://docs.yesodweb.com/book/persistent/
share2 mkPersist (mkMigrate "migrateAll") [$persist|
User
    ident String Asc
    password String Update
    UniqueUser ident
    deriving

Wiki
    path String Asc
    content String Update
    updated UTCTime Update Desc
    version Int default=0 Add
    editor UserId Update
    UniqueWiki path

WikiHistory
    wiki WikiId Eq
    path String Asc Eq
    content String
    updated UTCTime Desc
    version Int
    editor UserId
    deleted Bool default=false Eq
|]

-- manually insert first User
-- account  : kestrel
-- password : kestrel
-- insert into "User" (ident,password) values ('kestrel','ZXQGB6393dfa5c4023381310fb84c864debf7');
