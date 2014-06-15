module Backend.Provider where

import Backend.Common
import DBTransaction

import Database.HDBC.Types
import Data.List
import Data.Time.LocalTime

--Provider
type AsProvider a = AsSomeone a
---add new type of product
addType :: AsProvider ( 
  String -> String -> DBTransaction () )
addType _ name desc = runQuery 
  "insert into typ_produktu (nazwa, opis) values (?,?);" $ map toSql [name, desc]
---delete type of product he no longer can deliver
deleteType :: AsProvider ( Int -> DBTransaction () )
deleteType pro tp = runQuery "delete from dostarcza where tpid = ? and doid = ?;" $ map toSql [tp, pro]
---add new type of product he can deliver
addProvision :: AsProvider ( 
  Int -> Int -> DBTransaction () )
addProvision pro tp pr = deleteType pro tp >>
   (runQuery "insert into dostarcza (tpid, doid, cena) values (?,?,?);" $ 
    [toSql tp, toSql pro, toSql pr])
---see all types of products
allTypes :: AsProvider ( DBTransaction [String] )
allTypes a = liftDB (convertPrettifyAddHeader 
  ["id", "nazwa", "dostarczam"]) $
    query "select tpid, nazwa, count(cena) from typ_produktu left outer join \
    \ (select cena, tpid from dostarcza where doid = ?) X using (tpid) group by tpid, nazwa;" [toSql a]
---see all owners he provided for
listOwners :: AsProvider ( DBTransaction [String] )
listOwners pro = liftDB (convertPrettifyAddHeader
  ["typ_produktu", "kontakt"]) $
    query "select nazwa, mail from produkt join typ_produktu using (tpid) join wlasciciel using (wlid) \
      \ where doid = ?;" $ [toSql pro]
---see what he provides currently
listProvisions :: AsProvider (DBTransaction [String])
listProvisions pro = liftDB (convertPrettifyAddHeader
  ["id", "nazwa", "cena"]) $
    query "select tpid, nazwa, cena from typ_produktu join dostarcza using (tpid) \
    \ where doid = ?;" [toSql pro] 
