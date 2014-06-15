module Frontend.Owner where 

import Frontend.Common
import DBTransaction

import Graphics.Vty.Widgets.All
import Graphics.Vty.Attributes
import Graphics.Vty.LLInput

import qualified Data.Text as T
import Control.Monad
import Control.Monad.Reader (liftIO, lift)
import Data.Maybe (fromJust)

import Backend.Owner

ownerScreen = fromButtonList "Wlasciciel" $ screenChooser
  [ ( "Historia zamowien", ownerHistoryScreen),                -- > szczegly zamowienia
    ( "Niezrealizowane zamowienia", ownerUnrealizedScreen),       -- > zrealizuj zamowienie
    ( "Zobacz dostepne produkty", ownerAvailableScreen) ]        -- > kto je dostarcza -> dodaj produkt

ownerHistoryScreen = runInIO $ do 
  conn <- takeConn
  kuid <- takeRoInt
  tp <- takeTp 
  a <- listScreen "Historia zamowien" (setMVarAndGoto tp ownerOrderDetailsScreen) $ ((ownerHistory kuid `unDB` conn) >>= splitHeader)
  liftIO a

ownerOrderDetailsScreen = runInIO $ do 
  conn <- takeConn
  kuid <- takeRoInt
  zaid <- takeTpInt
  a <- listScreen "Szczegoly zamowienia" (doNothing) $ ((ownerOrderDetails kuid zaid `unDB` conn) >>= splitHeader)
  liftIO a

ownerUnrealizedScreen = runInIO $ do 
  conn <- takeConn
  kuid <- takeRoInt
  tp <- takeTp 
  a <- listScreen "Zrealizuj zamowienie" (setMVarAndGoto tp ownerRealizeOrderScreen) $ ((ownerUnrealized kuid `unDB` conn) >>= splitHeader)
  liftIO a

ownerRealizeOrderScreen = confirmationScreen "Zrealizowac to zamowienie?" "Zrealizowano zamowienie" $ \ku tp pr conn st -> 
  do
    kuid <- takeInt ku
    zaid <- takeInt tp
    (realizeOrder kuid zaid `unDB` conn)
    void $ popFromStack st
    void $ popFromStack st

ownerAvailableScreen = runInIO $ do 
  conn <- takeConn
  kuid <- takeRoInt
  tp <- takeTp 
  a <- listScreen "Dostepne produkty" (setMVarAndGoto tp ownerProvidersOfScreen) $ ((ownerAvailable kuid `unDB` conn) >>= splitHeader)
  liftIO a

ownerProvidersOfScreen = runInIO $ do 
  conn <- takeConn
  kuid <- takeRoInt
  tpid <- takeTpInt 
  pr <- takePr
  a <- listScreen "Dodaj produkt od dostawcy" (setMVarAndGoto pr ownerAddProductScreen) $ ((ownerProvidersOf kuid tpid `unDB` conn) >>= splitHeader)
  liftIO a

ownerAddProductScreen = confirmationScreen "Dodac produkt od tego dostawcy?" "Dodano produkt" $ \ku tp pr conn st -> 
  do
    kuid <- takeInt ku
    tpid <- takeInt tp
    doid <- takeInt pr 
    (addProduct kuid tpid doid `unDB` conn)
    void $ popFromStack st
    void $ popFromStack st
    void $ popFromStack st

