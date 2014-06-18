module Backend.Buyer where

import Backend.Common
import DBTransaction

import Database.HDBC.Types
import Data.List
import Data.Time.LocalTime

--Buyer
type AsBuyer a = AsSomeone a
---add new order
addOrder :: AsBuyer ( Int -> DBTransaction () )
addOrder buy own = runQuery "insert into zamowienie (kuid, wlid) values (?,?);" $ map toSql [buy, own]
---add given product to given order -- check if owner has this product and check if this buyer has this order
addProductToOrder :: AsBuyer ( Int -> Int -> DBTransaction () )
addProductToOrder buy ord pr = runQuery "select * from dodaj_do_zamowienia_jako(?,?,?);" $ 
   map toSql [buy, ord, pr]
---finish composing order -- check ownership of order
finishOrder :: AsBuyer (Int -> DBTransaction () )
finishOrder buy ord = runQuery "select * from zloz_zamowienie_jako(?,?);" $ map toSql [buy, ord]
---see history (orders)
buyerHistory :: AsBuyer ( DBTransaction [String] )
buyerHistory buy = liftDB (convertPrettifyAddHeader 
   ["id", "data_zlozenia", "data_realizacji", "wartosc", "sprzedajacy"]) $ 
    query ("select zaid, zlozenie, realizacja, wartosc, mail \
    \ from zamowienie join wlasciciel using (wlid) where kuid = ?;") [toSql buy]
--see details of order
buyerOrderDetails :: AsBuyer ( Int -> DBTransaction [String])
buyerOrderDetails buy ord = liftDB (convertPrettifyAddHeader 
   ["produkt","cena" ] ) $ 
    query "select nazwa,cena \
    \ from zamowienie join produkt using (zaid) join typ_produktu using (tpid) \
     \ where zaid = ?;"
     [toSql ord]
---list all unfinished orders
buyerUnfinished :: AsBuyer ( DBTransaction [String] )
buyerUnfinished buy = liftDB (convertPrettifyAddHeader 
   ["id", "wartosc", "sprzedajacy"]) $ 
    query ("select zaid, wartosc, mail \ 
    \ from zamowienie join wlasciciel using (wlid) where kuid = ? and zlozenie is null;") [toSql buy]
---list all products that can be added
buyerOptions :: AsBuyer ( DBTransaction [String] )
buyerOptions _ = liftDB (convertPrettifyAddHeader 
   ["id","nazwa"]) $ 
    query "select tpid,nazwa from typ_produktu join produkt using (tpid) \
    \  where zaid is null group by tpid, nazwa;" [] 
---see which owner has given product type
whoHasX :: AsBuyer ( Int -> DBTransaction [String] )
whoHasX _ tp = liftDB (convertPrettifyAddHeader 
   ["id produktu", "id wlasciciela", "cena", "kontakt"]) $ 
    query "select prid, wlid, cena, mail from produkt join wlasciciel using (wlid) where tpid = ? and zaid is null;" [toSql tp]
---see to which order can given product be added
buyerOrdersOfOwner :: AsBuyer ( Int -> DBTransaction [String])
buyerOrdersOfOwner buy pr = liftDB (convertPrettifyAddHeader 
  ["id", "wartosc" ]) $
    query "select zaid, wartosc from zamowienie where zlozenie is null and kuid = ?;" [toSql buy]
---see all owners
buyerOwners :: AsBuyer ( DBTransaction [String])
buyerOwners id = liftDB (convertPrettifyAddHeader
  ["id", "kontakt", "nieukonczone zamowienia"]) $
    query "select wlid, mail, count(zaid) from wlasciciel left outer join (select wlid, zaid from zamowienie where kuid = ? and zlozenie is null) X \
    \ using (wlid) group by wlid, mail;" [toSql id]
