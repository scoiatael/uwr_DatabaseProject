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
---add new provider
---add new owner

--Owner
---add new product
---set order to realized
---see his history (orders, customers and product number)
---see particular order
---see unrealized orders and customers contacts
---see all types of products that can be provided
---list providers who can deliver particular type of product
 
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
