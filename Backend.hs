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

type asSomeone a = Int -> a

--Admin
---add new buyer
addBuyer :: String -> String -> DBTransaction ()
addBuyer mail pass = query "insert into kupujacy (mail) values ?;" [toSql mail]
deleteBuyer :: Int -> DBTransaction ()
logAsBuyer :: Int -> String -> DBTransaction ()

---add new provider
addProvider :: String -> String -> DBTransaction ()
addProvider mail pass = query "insert into dostawca (mail) values ?;" [toSql mail]
deleteProvider :: Int -> DBTransaction ()
logAsProvider :: Int -> String -> DBTransaction ()

---add new owner
addOwner :: String -> Float -> String -> DBTransaction ()
addProvider mail markup pass = query "insert into wlasciciel (mail,marza) values (?,?);" $
  [toSql mail, toSql markup]
deleteOwner :: Int -> DBTransaction ()
logAsOwner :: Int -> String -> DBTransaction ()

--Owner
type asOwner a = asSomeone a
---add new product
addProduct :: asOwner ( 
  Int -> Int -> DBTransaction () )
addProduct own prd prv = runQuery "insert produkt (wlid, tpid, doid) values (?,?,?);" $ 
  map toSql [own,prd,prv]
---set order to realized -- check if thats his order
realizeOrder :: asOwner (
  Int -> DBTransaction () )
realizeOrder own ord = runQuery "select * from wykonaj_zamowienie_jako(?,?);" $ map toSql [own, ord]
---see his history (orders, customers)
ownerHistory :: asOwner (
  DBTransaction [String] )
ownerHistory own = liftDB (convertPrettifyAddHeader \
  \ ["data_zlozenia", "data_realizacji", "wartosc", "kupujacy" ]) $ \
   \ query ( "select zlozenie, realizacja, wartosc, mail " ++ \ 
    \ "from zamowienie join kupujacy using (kuid) where wlid = ?;") [toSql own]

---see particular order
ownerOrderDetails :: asOwner (
  Int -> DBTransaction [String] )
ownerOrderDetails own ord = liftDB (convertPrettifyAddHeader \
  \ ["nr produktu", "produkt"] ) $ \
   \ query "select * from zamowienie join produkt using (zaid) where wlid = ? and zamowienie.wlid = ?;" \
    \ [toSql own, toSql ord]
---see unrealized orders and customers contacts
ownerUnrealized :: asOwner ( 
  DBTransaction [String] )
ownerUnrealized own = liftDB (convertPrettifyAddHeader \
  \ ["data_zlozenia", "wartosc", "kupujacy" ]) $ \
   \ query ( "select zlozenie, wartosc, mail " ++ \ 
    \ ("from zamowienie join kupujacy using (kuid) " ++ 
     \ "where wlid = ? and realizacja is null and zlozenie is not null;")) [toSql own]
---see all types of products that can be provided
ownerAvailible :: asOwner (
  DBTransaction [String] )
ownerAvailible _ = liftDB (convertPrettifyAddHeader \
  \ ["nazwa", "opis"]) $ \
   \ query "select nazwa, opis from typ_produktu;"
---list providers who can deliver particular type of product
listProviders :: asOwner (
  Int -> DBTransaction [String] )
listProviders _ tp = liftDB (convertPrettifyAddHeader \
  \ ["cena", "dostawca"]) $ \
   \ query "select cena, mail from dostarcza join dostawca using (doid) where tpid = ?;" [toSql tp]
 
--Buyer
type asBuyer a = asSomeone a
---add new order
addOrder :: asBuyer ( Int -> DBTransaction () )
addOrder buy own = runQuery "insert into zamowienie (kuid, wlid) values (?,?);" $ map toSql [buy, own]
---add given product to given order -- check if owner has this product and check if this buyer has this order
addProductToOrder :: asBuyer ( Int -> Int -> DBTransaction () )
addProductToOrder buy ord pr = runQuery "select * from dodaj_do_zamowienia_jako(?,?,?);" $ \
  \ map toSql [buy, ord, pr]
---finish composing order -- check ownership of order
finishOrder :: asBuyer (Int -> DBTransaction () )
finishOrder buy ord = runQuery "select * from zloz_zamowienie_jako(?,?);" $ map toSql [buy, ord]
---see history (orders)
buyerHistory :: asBuyer ( DBTransaction [String] )
buyerHistory buy = liftDB (convertPrettifyAddHeader \
  \ ["data_zlozenia", "data_realizacji", "wartosc", "sprzedajacy"]) $ \
   \ query ("select zlozenie, realizacja, wartosc, mail" ++ \ 
    \ " from zamowienie join wlasciciel using (wlid) where kuid = ?;") [toSql buy]
--see details of order
buyerOrderDetails = asBuyer ( Int -> DBTransaction [String])
buyerOrderDetails buy ord = liftDB (convertPrettifyAddHeader \
  \ ["nr produktu", "produkt"] ) $ \
   \ query "select * from zamowienie join produkt using (zaid) where zaid = ? and kuid = ?;" \
    \ [toSql ord, toSql buy]
---list all unfinished orders
buyerUnfinished :: asBuyer ( DBTransaction [String] )
buyerUnfinished buy = liftDB (convertPrettifyAddHeader \
  \ ["data_zlozenia", "wartosc", "sprzedajacy"]) $ \
   \ query ("select wartosc, mail" ++ \ 
    \ " from zamowienie join wlasciciel using (wlid) where kuid = ? and zlozenie is null;") [toSql buy]
---list all products that can be added to given order
buyerOptions :: asBuyer ( Int -> DBTransaction [String] )
buyerOptions _ ord = liftDB (convertPrettifyAddHeader \
  \ ["nazwa", "opis"]) $ \
   \ query ("select nazwa, opis from typ_produktu join " ++ \
    \ "(select * from produkt join " ++ \
     \ "(select wlid from zamowienie where zaid = ?) X " ++ \
      \ "using (wlid) where zaid is null) Y using (tpid);") [toSql ord]
---see all types of products
buyerAllTypes :: asBuyer ( DBTransaction [String] )
buyerAllTypes a = ownerAvailible a
---see which owner has given product type
whoHasX :: asBuyer ( Int -> DBTransaction [String] )
whoHasX _ = liftDB (convertPrettifyAddHeader \
  \ ["cena", "kontakt"]) $ \
   \ query "select cena, mail from produkt join wlasciciel using (wlid) where tpid = ?;"

--Provider
type asProvider a = asSomeone a
---add new type of product
addType :: asProvider ( 
  String -> String -> DBTransaction () )
addType _ name desc = query "insert into typ_produktu (nazwa, opis) values (?,?);" $ map toSql [name, desc]
---delete type of product he no longer can deliver
deleteType :: asProvider ( Int -> DBTransaction () )
deleteType pro tp = query "delete from dostarcza where tpid = ? and doid = ?;" $ map toSql [tp, pro]
---add new type of product he can deliver
addProvision :: asProvider ( 
  Int -> Int -> DBTransaction () )
addProvision pro tp pr = deleteType pro tp >> \
  \ query "insert into dostarcza (tpid, doid, cena) values (?,?,?);" $ \
   \ [toSql tp, toSql pro, toSql pr]
---see all types of products
allTypes :: asProvider ( DBTransaction [String] )
allTypes a = ownerAvailible a

runT = runTransaction
