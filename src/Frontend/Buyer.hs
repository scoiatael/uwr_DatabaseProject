module Frontend.Buyer where 

import Frontend.Common
import DBTransaction

import Graphics.Vty.Widgets.All
import Graphics.Vty.Attributes
import Graphics.Vty.LLInput

import qualified Data.Text as T
import Control.Monad
import Control.Monad.Reader (liftIO, lift)
import Data.Maybe (fromJust)

import Backend.Buyer

buyerScreen = fromButtonList "Kupujacy" $ screenChooser
  [ ( "Historia zamowien", buyerHistoryScreen),                -- > szczegoly zamowienia
    ( "Zobacz produkty", buyerProductScreen ),                 -- > zobacz kto posiada -- > dodaj do zamowienia
    ( "Zobacz sprzedawcow", buyerOwnerScreen ),              -- > dodaj zamowienie
    ( "Zobacz niezlozone zamowienia", buyerUnfinishedOrdScreen ) ]   -- > zloz zamowienie

buyerHistoryScreen = runInIO $ do
  conn <- takeConn
  rN <- takeRoInt
  tp <- takeTp 
  a <- listScreen "Historia zamowien" (setMVarAndGoto tp buyerOrderDetailsScreen) $ ((buyerHistory rN `unDB` conn) >>= splitHeader)
  liftIO a

buyerOrderDetailsScreen = runInIO $ do
  conn <- takeConn
  kuid <- takeRoInt
  zaid <- takeTpInt
  a <- listScreen "Szczegoly zamowienia" (doNothing) $ ((buyerOrderDetails kuid zaid `unDB` conn) >>= splitHeader)
  liftIO a

buyerProductScreen = runInIO $ do
  conn <- takeConn
  kuid <- takeRoInt
  tp <- takeTp 
  a <- listScreen "Dostepne produkty" (setMVarAndGoto tp buyerSeeOwnerOfScreen) $ ((buyerOptions kuid `unDB` conn) >>= splitHeader)
  liftIO a

buyerSeeOwnerOfScreen = runInIO $ do
  conn <- takeConn
  kuid <- takeRoInt
  tpid <- takeTpInt 
  pr <- takePr
  a <- listScreen "Wlasciciele produktow" (setMVarAndGoto pr buyerOrderScreen) $ ((whoHasX kuid tpid `unDB` conn) >>= splitHeader)
  liftIO a
  
buyerOrderScreen = runInIO $ do
  conn <- takeConn
  kuid <- takeRoInt
  tp <- takeTp 
  prid <- takePrInt
  a <- listScreen "Dodaj do zamowienia" (setMVarAndGoto tp buyerAddToOrderScreen) $ ((buyerOrdersOfOwner kuid prid `unDB` conn) >>= splitHeader)
  liftIO a
  
buyerAddToOrderScreen = runInIO $ do
  conn <- takeConn
  kuid <- takeRoInt
  zaid <- takeTpInt
  prid <- takePrInt 
  st <- takeStack
  conn <- takeConn
 
  nextScreen <- infoScreen "Dodano do zamowienia." 
  catchErr <- errScreen
  ok <- makeButton ("Ok", catchErr `inThis` 
    do
      (addProductToOrder kuid zaid prid `unDB` conn)
      void $ popFromStack st
      void $ popFromStack st
      void $ popFromStack st
      void $ popFromStack st
      nextScreen
    )
  b <- backButton
  tytul <- simpleText "Czy chcesz dodac do tego zamowienia?"
  fg <- simpleFocusGroup
  addWidgetToFG fg ok
  addWidgetToFG fg b
     
  cw <- liftIO $ (centered tytul) <--> (centered ok) <--> (centered b)
  a <- addCollection cw fg
  liftIO a 
  

buyerOwnerScreen = runInIO $ do
  conn <- takeConn
  tp <- takeTp 
  kuid <- takeRoInt
  a <- listScreen "Dodaj zamowienie do wlasciciela" (setMVarAndGoto tp buyerAddOrderScreen) $ ((buyerOwners kuid `unDB` conn) >>= splitHeader)
  liftIO a

buyerAddOrderScreen = confirmationScreen "Dodac zamowienie do tego sprzedajacego?" "Dodano zamowienie" $ 
  \ku tp pr conn st -> 
    do
      kuid <- takeInt ku
      wlid <- takeInt tp
      (addOrder kuid wlid `unDB` conn)
      void $ popFromStack st
      void $ popFromStack st
    
buyerUnfinishedOrdScreen = runInIO $ do 
  conn <- takeConn
  kuid <- takeRoInt
  tp <- takeTp 
  a <- listScreen "Zloz zamowienie" (setMVarAndGoto tp buyerFinishOrderScreen) $ ((buyerUnfinished kuid `unDB` conn) >>= splitHeader)
  liftIO a

buyerFinishOrderScreen = confirmationScreen "Zlozyc to zamowienie?" "Zlozono zamowienie" $ \ku tp pr conn st -> 
  do
    kuid <- takeInt ku
    zaid <- takeInt tp
    (finishOrder kuid zaid `unDB` conn)
    void $ popFromStack st
    void $ popFromStack st

