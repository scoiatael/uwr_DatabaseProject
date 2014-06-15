module Backend.Owner where

import Backend.Common

import DBTransaction
import Database.HDBC.Types
import Data.List
import Data.Time.LocalTime

--Owner
type AsOwner a = AsSomeone a
---add new product
addProduct :: AsOwner ( 
  Int -> Int -> DBTransaction () )
addProduct own prd prv = runQuery "insert into produkt (wlid, tpid, doid) values (?,?,?);" $ 
  map toSql [own,prd,prv]
---set order to realized -- check if thats his order
realizeOrder :: AsOwner (
  Int -> DBTransaction () )
realizeOrder own ord = runQuery "select * from wykonaj_zamowienie_jako(?,?);" $ map toSql [own, ord]
---see his history (orders, customers)
ownerHistory :: AsOwner (
  DBTransaction [String] )
ownerHistory own = liftDB (convertPrettifyAddHeader 
   ["id", "data_zlozenia", "data_realizacji", "wartosc", "kupujacy" ]) $ 
    query ( "select zaid, zlozenie, realizacja, wartosc, mail \
     \ from zamowienie join kupujacy using (kuid) where wlid = ?;") [toSql own]
---see particular order
ownerOrderDetails :: AsOwner (
  Int -> DBTransaction [String] )
ownerOrderDetails own ord = liftDB (convertPrettifyAddHeader 
   ["produkt","cena" ] ) $ 
    query "select nazwa,cena \
    \ from zamowienie join produkt using (zaid) join typ_produktu using (tpid) \
     \ where zamowienie.wlid = ? and zaid = ?;"
     [toSql own, toSql ord]
---see unrealized orders and customers contacts
ownerUnrealized :: AsOwner ( 
  DBTransaction [String] )
ownerUnrealized own = liftDB (convertPrettifyAddHeader 
   ["id","data_zlozenia", "wartosc", "kupujacy" ]) $ 
    query ( "select zaid,zlozenie, wartosc, mail \ 
    \ from zamowienie join kupujacy using (kuid) \
     \ where wlid = ? and realizacja is null and zlozenie is not null;") [toSql own]
---see all types of products that can be provided
ownerAvailable :: AsOwner (
  DBTransaction [String] )
ownerAvailable wlid = liftDB (convertPrettifyAddHeader
   ["id","nazwa", "posiadasz"]) $ 
    query "select tpid, nazwa, count(prid) from typ_produktu left outer join \
    \ (select prid, tpid from produkt where wlid = ? and zaid is null) X using (tpid) group by tpid, nazwa;" [toSql wlid]
---list providers who can deliver particular type of product
ownerProvidersOf :: AsOwner (
  Int -> DBTransaction [String] )
ownerProvidersOf _ tp = liftDB (convertPrettifyAddHeader 
   ["id", "cena", "dostawca"]) $ 
    query "select doid, cena, mail from dostarcza join dostawca using (doid) where tpid = ?;" [toSql tp]
---
