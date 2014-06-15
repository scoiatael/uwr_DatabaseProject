module Backend.Common where

import DBTransaction
import Database.HDBC.Types
import Data.List
import Data.Time.LocalTime

mFromSql SqlNull = "Null"
mFromSql (SqlRational r) = show $ ((fromRational r) :: Double)
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

resetRole = runQuery "reset role;" []
logAsBuyer :: Int -> String -> DBTransaction ()
logAsBuyer i _ = runQuery "select * from zaloguj_jako_kupujacy(?);" [toSql i]
logAsProvider :: Int -> String -> DBTransaction ()
logAsProvider i _ = runQuery "select * from zaloguj_jako_dostawca(?);" [toSql i]
logAsOwner :: Int -> String -> DBTransaction ()
logAsOwner i _ = runQuery "select * from zaloguj_jako_wlasciciel(?);" [toSql i]
  
