module Backend where

import DBTransaction
import Database.HDBC.Types
import Data.List
import Data.Time.LocalTime

mFromSql SqlNull = "Null"
mFromSql a = fromSql a

convertPrettifyAddHeader :: [String] -> [[SqlValue]] -> [String]
convertPrettifyAddHeader head sqls = map (concat) $ 
  prettifyStringTable $ head : map (map mFromSql) sqls 

prettifyStringTable :: [[String]] -> [[String]]
prettifyStringTable t = transpose $ map prettifyStringList $ zip [0..]  $ transpose t

prettifyStringList :: (Int, [String]) -> [String]
prettifyStringList (a,l) = let 
  mlength = foldl (\a b -> let lb = length b in if lb > a then lb else a) 0 l
  in map (\s -> (if a == 0 then "" else " | ") ++ padTo mlength s) l

padTo :: Int -> String -> String
padTo l s = s ++ replicate (l - length s) ' '

type AsSomeone a = Int -> a

--Admin
---add new buyer
addBuyer :: String -> String -> DBTransaction ()
addBuyer mail pass = runQuery "insert into kupujacy (mail) values ?;" [toSql mail]
logAsBuyer :: Int -> String -> DBTransaction ()
logAsBuyer _ _ = runQuery "set role 'buyer';" []

---add new provider
addProvider :: String -> String -> DBTransaction ()
addProvider mail pass = runQuery "insert into dostawca (mail) values ?;" [toSql mail]
logAsProvider :: Int -> String -> DBTransaction ()
logAsProvider _ _ = runQuery "set role 'provider';" []

---add new owner
addOwner :: String -> Rational -> String -> DBTransaction ()
addOwner mail markup pass = runQuery "insert into wlasciciel (mail,marza) values (?,?);" $
  [toSql mail, toSql markup]
logAsOwner :: Int -> String -> DBTransaction ()
logAsOwner _ _ = runQuery "set role 'owner';" []

resetRole = query "reset role;" []

--Owner
type AsOwner a = AsSomeone a
---add new product
addProduct :: AsOwner ( 
  Int -> Int -> DBTransaction () )
addProduct own prd prv = runQuery "insert produkt (wlid, tpid, doid) values (?,?,?);" $ 
  map toSql [own,prd,prv]
---set order to realized -- check if thats his order
realizeOrder :: AsOwner (
  Int -> DBTransaction () )
realizeOrder own ord = runQuery "select * from wykonaj_zamowienie_jako(?,?);" $ map toSql [own, ord]
---see his history (orders, customers)
ownerHistory :: AsOwner (
  DBTransaction [String] )
ownerHistory own = liftDB (convertPrettifyAddHeader 
   ["data_zlozenia", "data_realizacji", "wartosc", "kupujacy" ]) $ 
    query ( "select zlozenie, realizacja, wartosc, mail \
     \ from zamowienie join kupujacy using (kuid) where wlid = ?;") [toSql own]

---see particular order
ownerOrderDetails :: AsOwner (
  Int -> DBTransaction [String] )
ownerOrderDetails own ord = liftDB (convertPrettifyAddHeader 
   ["nr produktu", "produkt"] ) $ 
    query "select * from zamowienie join produkt using (zaid) where wlid = ? and zamowienie.wlid = ?;"
     [toSql own, toSql ord]
---see unrealized orders and customers contacts
ownerUnrealized :: AsOwner ( 
  DBTransaction [String] )
ownerUnrealized own = liftDB (convertPrettifyAddHeader 
   ["data_zlozenia", "wartosc", "kupujacy" ]) $ 
    query ( "select zlozenie, wartosc, mail \ 
    \ from zamowienie join kupujacy using (kuid) \
     \ where wlid = ? and realizacja is null and zlozenie is not null;") [toSql own]
---see all types of products that can be provided
ownerAvailible :: AsOwner (
  DBTransaction [String] )
ownerAvailible _ = liftDB (convertPrettifyAddHeader
   ["nazwa", "opis"]) $ 
    query "select nazwa, opis from typ_produktu;" []
---list providers who can deliver particular type of product
listProviders :: AsOwner (
  Int -> DBTransaction [String] )
listProviders _ tp = liftDB (convertPrettifyAddHeader 
   ["cena", "dostawca"]) $ 
    query "select cena, mail from dostarcza join dostawca using (doid) where tpid = ?;" [toSql tp]
 
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
   ["data_zlozenia", "data_realizacji", "wartosc", "sprzedajacy"]) $ 
    query ("select zlozenie, realizacja, wartosc, mail \
    \ from zamowienie join wlasciciel using (wlid) where kuid = ?;") [toSql buy]
--see details of order
buyerOrderDetails :: AsBuyer ( Int -> DBTransaction [String])
buyerOrderDetails buy ord = liftDB (convertPrettifyAddHeader 
   ["nr produktu", "produkt"] ) $ 
    query "select * from zamowienie join produkt using (zaid) where zaid = ? and kuid = ?;" 
     [toSql ord, toSql buy]
---list all unfinished orders
buyerUnfinished :: AsBuyer ( DBTransaction [String] )
buyerUnfinished buy = liftDB (convertPrettifyAddHeader 
   ["data_zlozenia", "wartosc", "sprzedajacy"]) $ 
    query ("select wartosc, mail \ 
    \ from zamowienie join wlasciciel using (wlid) where kuid = ? and zlozenie is null;") [toSql buy]
---list all products that can be added to given order
buyerOptions :: AsBuyer ( Int -> DBTransaction [String] )
buyerOptions _ ord = liftDB (convertPrettifyAddHeader 
   ["nazwa", "opis"]) $ 
    query ("select nazwa, opis from typ_produktu join \ 
    \ (select * from produkt join \ 
     \ (select wlid from zamowienie where zaid = ?) X \ 
      \ using (wlid) where zaid is null) Y using (tpid);") [toSql ord]
---see all types of products
buyerAllTypes :: AsBuyer ( DBTransaction [String] )
buyerAllTypes a = ownerAvailible a
---see which owner has given product type
whoHasX :: AsBuyer ( Int -> DBTransaction [String] )
whoHasX _ tp = liftDB (convertPrettifyAddHeader 
   ["cena", "kontakt"]) $ 
    query "select cena, mail from produkt join wlasciciel using (wlid) where tpid = ?;" [toSql tp]

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
allTypes a = ownerAvailible a

runT = runTransaction
