import FrontendHelpers
import Backend
import DBTransaction

import Graphics.Vty.Widgets.All
import Graphics.Vty.Attributes
import Graphics.Vty.LLInput

import qualified Data.Text as T
import Control.Monad
import Control.Monad.Reader (liftIO, lift)
import Data.Maybe (fromJust)

main :: IO ()
main = runFrontEnd start

start =  do
  fg <- simpleFocusGroup 
  st <- takeStack
  conn <- takeConn
  next <- chooseRole
  ok <- makeButton ("Logowanie",
    do
      (resetRole `unDB` conn) 
      next
    )
  tytul <- simpleText $ "Witaj w CoffeeShopie "
  b <- backButton
  addWidgetToFG fg ok
  addWidgetToFG fg b
  cw <- liftIO $ (centered tytul) <--> (centered ok) <--> (centered b)
  a <- addCollection cw fg
  liftIO a

chooseRole = fromButtonList "Zaloguj sie jako" $ screenChooser
  [ ( "Admin",  adminScreen),
    ( "Kupujacy", loginScreen "Kupujacy" logAsBuyer buyerScreen),
    ( "Dostawca", loginScreen "Dostawca" logAsProvider providerScreen),
    ( "Wlasciciel", loginScreen "Wlasciciel" logAsOwner ownerScreen) ]

adminScreen = fromButtonList "Admin" $ screenChooser
  [ ("Dodaj uzytkownika", addUserScreen),
    ("Stworz baze", createDataBase),
    ("Zobacz kupujacych", listBuyerScreen),
    ("Zobacz wlascicieli", listOwnerScreen),
    ("Zobacz dostawcow", listProviderScreen),
    ("Usun histore", createDataBase) ]

listProviderScreen = do
  conn <- takeConn
  listScreen "Dostawcy" (doNothing) $ ((listAllProviders `unDB` conn) >>= splitHeader)
listOwnerScreen = do
  conn <- takeConn
  listScreen "Wlasciciele" (doNothing) $ ((listAllOwners `unDB` conn) >>= splitHeader)
listBuyerScreen = do
  conn <- takeConn
  listScreen "Kupujacy" (doNothing) $ ((listAllBuyers `unDB` conn) >>= splitHeader)

loginScreen n log next = do
  fg <- simpleFocusGroup 
  back <- backButton
  name <- createEditLineAddFG fg
  conn <- takeConn
  nextScreen <- next
  onErr <- errScreen
  errScreen <- infoScreen "ID powinno byc liczba!"
  st <- takeStack
  ro <- takeRo
  ok <- makeButton ("Ok",
    do
      txt <- liftM T.unpack $ getEditText name
      let n = intFromString txt
      if n == Nothing 
        then errScreen
        else onErr `inThis` do
          (log (fromJust n) "" `unDB` conn) 
          void $ popFromStack st
          void $ popFromStack st
          pushInt ro $ fromJust n
          nextScreen
    )
  tytul <- simpleText $ "Logowanie - " ++ n
  addWidgetToFG fg ok
  addWidgetToFG fg back
  nGr <- addText "ID:" name
  cw <- liftIO $ (centered tytul) <--> (centered nGr) <--> (centered ok) <--> (centered back)
  addCollection cw fg

createDataBase = runInIO $ do
  lift $ resetDatabase
  a <- infoScreen "Zrobione."
  liftIO a

addUserScreen = fromButtonList "Dodaj..." $ screenChooser 
  [ ("..kupujacego", addUserSimpleScreen "kupujacy" addBuyer ),
    ("..wlasciciela", addOwnerScreen ),
    ("..dostawce", addUserSimpleScreen "dostawca" addProvider) ]
   
addOwnerScreen = do 
  fg <- simpleFocusGroup 
  back <- backButton
  name <- createEditLineAddFG fg
  marz <- createEditLineAddFG fg
  conn <- takeConn
  onErr <- errScreen
  nextScreen <- infoScreen "Dodano uzytkownika."
  errScreen <- infoScreen "Marza powinna byc liczba postaci a / b"
  st <- takeStack
  ok <- makeButton ("Ok",
    do
      txt <- liftM T.unpack $ getEditText name
      mar <- liftM T.unpack $ getEditText marz
      let n = floatFromString $ map (\a -> if a == ',' then '.' else a) mar
      if n == Nothing 
        then errScreen
        else onErr `inThis` do
          (addOwner txt (fromJust n) "" `unDB` conn) 
          void $ popFromStack st
          nextScreen
    )
  tytul <- simpleText $ "Dodawanie uzytkownika - wlasciciel"
  addWidgetToFG fg ok
  addWidgetToFG fg back
  nGr <- addText "mail:" name
  mGr <- addText "marza:" marz
  cw <- liftIO $ (centered tytul) <--> (centered nGr) <--> 
    (centered mGr) <--> (centered ok) <--> (centered back)
  addCollection cw fg

addUserSimpleScreen n f = do
  fg <- simpleFocusGroup 
  back <- backButton
  name <- createEditLineAddFG fg
  conn <- takeConn
  nextScreen <- infoScreen "Dodano uzytkownika."
  st <- takeStack
  onErr <- errScreen
  ok <- makeButton ("Ok", onErr `inThis` 
    do
      txt <- liftM T.unpack $ getEditText name
      (f txt "" `unDB` conn)
      void $ popFromStack st
      nextScreen
    )
  tytul <- simpleText $ "Dodawanie uzytkownika - " ++ n
  addWidgetToFG fg ok
  addWidgetToFG fg back
  nGr <- addText "mail:" name
  cw <- liftIO $ (centered tytul) <--> (centered nGr) <--> (centered ok) <--> (centered back)
  addCollection cw fg

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

buyerAddOrderScreen = confirmationScreen "Dodac zamowienie do tego sprzedajacego?" "Dodano zamowienie" $ \ku tp pr conn st -> 
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


providerScreen = fromButtonList "Dostawca" $ screenChooser
  [ ( "Wspolpracownicy", providerListOwnerScreen),
    ( "Co dostarczam", providerListProvisionScreen),                    -- > przestan dostarczac
    ( "Typy produktow", providerAllTypeScreen),                   -- > zacznij dostarczac
    ( "Dodaj nowy typ produktu", providerAddTypeScreen) ]

providerListOwnerScreen =  runInIO $ do 
  conn <- takeConn
  kuid <- takeRoInt
  a <- listScreen "Wspolpracownicy" (doNothing) $ ((listOwners kuid `unDB` conn) >>= splitHeader)
  liftIO a

providerListProvisionScreen = runInIO $ do 
  conn <- takeConn
  kuid <- takeRoInt
  tp <- takeTp
  a <- listScreen "Co dostarczam" (setMVarAndGoto tp providerDeleteProvisionScreen) $ ((listProvisions kuid `unDB` conn) >>= splitHeader)
  liftIO a

providerDeleteProvisionScreen =  confirmationScreen "Przestac dostarczac ten produkt?" "Usunieto produkt" $ \ku tp pr conn st -> 
  do
    kuid <- takeInt ku
    tpid <- takeInt tp
    (deleteType kuid tpid `unDB` conn)
    void $ popFromStack st
    void $ popFromStack st

providerAllTypeScreen = runInIO $ do 
  conn <- takeConn
  kuid <- takeRoInt
  tp <- takeTp
  a <- listScreen "Zacznij dostarczac" (setMVarAndGoto tp providerAddProvisionScreen) $ ((allTypes kuid `unDB` conn) >>= splitHeader)
  liftIO a

providerAddProvisionScreen = runInIO $ do
  fg <- simpleFocusGroup 
  back <- backButton
  name <- createEditLineAddFG fg
  conn <- takeConn
  nextScreen <- infoScreen "Dodano nowy dostarczany produkt"
  catchErr <- errScreen
  errScreen <- infoScreen "Cena powinno byc liczba!"
  st <- takeStack
  kuid <- takeRoInt
  tpid <- takeTpInt
  ok <- makeButton ("Ok",
    do
      txt <- liftM T.unpack $ getEditText name
      let n = intFromString txt
      if n == Nothing 
        then errScreen
        else catchErr `inThis` do
          (addProvision kuid tpid (fromJust n) `unDB` conn) 
          void $ popFromStack st
          void $ popFromStack st
          nextScreen
    )
  tytul <- simpleText $ "Dostarczanie produktu"
  addWidgetToFG fg ok
  addWidgetToFG fg back
  nGr <- addText "Cena:" name
  cw <- liftIO $ (centered tytul) <--> (centered nGr) <--> (centered ok) <--> (centered back)
  a <- addCollection cw fg
  liftIO a

providerAddTypeScreen = runInIO $ do
  fg <- simpleFocusGroup 
  back <- backButton
  name <- createEditLineAddFG fg
  desc <- createEditMultiLineAddFG fg
  conn <- takeConn
  nextScreen <- infoScreen "Dodano nowy typ produktu"
  catchErr <- errScreen
  st <- takeStack
  kuid <- takeRoInt
  ok <- makeButton ("Ok", catchErr `inThis` 
    do
      txt <- liftM T.unpack $ getEditText name
      dsc <- liftM T.unpack $ getEditText desc
      (addType kuid txt dsc `unDB` conn) 
      void $ popFromStack st
      void $ popFromStack st
      nextScreen
    )
  tytul <- simpleText $ "Nowy typ produktu"
  addWidgetToFG fg ok
  addWidgetToFG fg back
  nGr <- addText "Nazwa:" name
  dGr <- addText "Opis:" desc
  cw <- liftIO $ (centered tytul) <--> (centered nGr) <--> (centered dGr) <--> (centered ok) <--> (centered back)
  a <- addCollection cw fg
  liftIO a
