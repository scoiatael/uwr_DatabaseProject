module DBTransaction where

import Control.Monad
import Control.Monad.Reader

import Database.HDBC
import Database.HDBC.PostgreSQL

import System.Process

import qualified Data.Text as T

databaseName = "projekt.db"
adminName = "postgres"
baseName = "projekt.sql"
openOptions = "dbname=" ++ databaseName ++ " user=" ++ adminName

openDatabase = connectPostgreSQL openOptions

type DBTransactionT = ReaderT Connection
type DBTransaction = DBTransactionT IO 

liftDB :: (a -> b) -> DBTransaction a -> DBTransaction b
liftDB = liftM

conn :: DBTransaction Connection
conn = ask

unDB :: DBTransaction a -> Connection -> IO a
unDB act c = do
  v <- c `withTransaction` runReaderT act
--  commit c
  return v

runTransaction :: DBTransaction a -> IO a
runTransaction act = do
  c <- openDatabase
  val <- unDB act c
--  err <- disconnect c
  return val

runInTrans :: DBTransaction a -> DBTransaction (IO a)
runInTrans act = do
  c <- ask
  return $ unDB act c

withConn :: (Connection -> IO a) -> DBTransaction a
withConn a = do
  c <- conn
  liftIO $ a c

runQuery :: String -> [SqlValue] -> DBTransaction ()
runQuery s v = withConn $ \c -> do 
  run c s v
  return ()

query :: String -> [SqlValue] -> DBTransaction [[SqlValue]]
query s v = withConn $ \c -> quickQuery' c s v

revert :: DBTransaction ()
revert = withConn rollback

resetDatabase :: DBTransaction ()
resetDatabase = liftIO $ void $ runCommand 
  ("psql " ++ databaseName ++ " -U " ++ adminName ++ " < " ++ baseName ++ " > log.info 2&> log.err" )
