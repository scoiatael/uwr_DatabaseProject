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
newBuyer :: String -> String -> DBTransaction ()
deleteBuyer :: Int -> DBTransaction ()
logAsBuyer :: Int -> String -> DBTransaction ()

---add new provider
addProvider :: String -> String -> DBTransaction ()
deleteProvider :: Int -> DBTransaction ()
logAsProvider :: Int -> String -> DBTransaction ()

---add new owner
addOwner :: String -> String -> DBTransaction ()
deleteOwner :: Int -> DBTransaction ()
logAsOwner :: Int -> String -> DBTransaction ()

--Owner
type asOwner a = asSomeone a
---add new product
addProduct :: asOwner ( 
  Int -> Int -> DBTransaction () )
---set order to realized -- check if thats his order
realizeOrder :: asOwner (
  Int -> DBTransaction () )
---see his history (orders, customers and product number)
ownerHistory :: asOwner (
  DBTransaction [String] )
---see particular order
ownerOrderDetails :: asOwner (
  DBTransaction [String] )
---see unrealized orders and customers contacts
ownerUnrealized :: asOwner ( 
  DBTransaction [String] )
---see all types of products that can be provided
ownerAvailible :: asOwner (
  DBTransaction [String] )
---list providers who can deliver particular type of product
listProviders :: asOwner (
  Int -> DBTransaction [String] )
 
--Buyer
type asBuyer a = asSomeone a
---add new order
addOrder :: asBuyer ( Int -> DBTransaction () )
---add given product to given order -- check if owner has this product and check if this buyer has this order
addProductToOrder :: asBuyer ( Int -> Int -> DBTransaction () )
---finish composing order -- check ownership of order
finishOrder :: asBuyer (Int -> DBTransaction () )
---see history (orders)
buyerHistory :: asBuyer ( DBTransaction [String] )
---list all unfinished orders
buyerUnfinished :: asBuyer ( DBTransaction [String] )
---list all products that can be added to given order
buyerOptions :: asBuyer ( Int -> DBTransaction [String] )
---see all types of products
buyerAllTypes :: asBuyer ( DBTransaction [String] )
---see which owner has given product
whoHasX :: asBuyer ( Int -> DBTransaction [String] )

--Provider
type asProvider a = asSomeone a
---add new type of product
addType :: asProvider ( 
  String -> String -> DBTransaction () )
---add new type of product he can deliver
addProvision :: asProvider ( 
  Int -> Int -> DBTransaction () )
---delete type of product he no longer can deliver
deleteType :: asProvider ( Int -> DBTransaction () )
---see all types of products
allTypes :: asProvider ( DBTransaction [String] )

runT = runTransaction
