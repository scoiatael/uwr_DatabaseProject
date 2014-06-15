module Backend.Admin where

import Backend.Common
import DBTransaction

import Database.HDBC.Types
import Data.List
import Data.Time.LocalTime

--Admin
---add new buyer
addBuyer :: String -> String -> DBTransaction ()
addBuyer mail pass = runQuery "insert into kupujacy (mail) values (?);" [toSql mail]
listAllBuyers = liftDB (convertPrettifyAddHeader
  ["id", "mail"]) $
  query "select * from kupujacy;" []

---add new provider
addProvider :: String -> String -> DBTransaction ()
addProvider mail pass = runQuery "insert into dostawca (mail) values (?);" [toSql mail]
listAllProviders = liftDB (convertPrettifyAddHeader
  ["id", "mail"]) $
  query "select * from dostawca;" []

---add new owner
addOwner :: String -> Double -> String -> DBTransaction ()
addOwner mail markup pass = runQuery "insert into wlasciciel (mail,marza) values (?,?);" $
  [toSql mail, toSql markup]
listAllOwners = liftDB (convertPrettifyAddHeader
  ["id", "mail", "marza"]) $
  query "select * from wlasciciel;" []
