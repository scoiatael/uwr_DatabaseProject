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

--Admin
---add new buyer
newBuyer :: String -> DBTransaction ()
---add new provider
addProvider :: String -> DBTransaction ()
---add new owner
addOwner :: String -> DBTransaction ()

--Owner
---add new product
addProduct :: Int -> Int -> Int -> Int-> DBTransaction ()
---set order to realized
realizeOrder :: Int -> DBTransaction ()
---see his history (orders, customers and product number)
ownerHistory :: Int -> DBTransaction [String]
---see particular order
ownerOrderDetails :: Int -> DBTransaction [String]
---see unrealized orders and customers contacts
unrealized :: Int -> DBTransaction [String]
---see all types of products that can be provided
ownerAvailible :: Int -> DBTransaction [String]
---list providers who can deliver particular type of product
listProviders :: Int -> DBTransaction [String]
 
--Buyer
---add new order
---see history (orders)
---list all unrealized orders
---list all products that can be added to given order
---add given product to given order
---finish composing order
---see all types of products
---see which owner has given product

--Provider
---see all owners
---see all types of products
---add new type of product he can deliver
---delete type of product he no longer can deliver

runT = runTransaction
