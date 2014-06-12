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
main = runFrontEnd $ do
  w <- fromButtonList "Zaloguj sie jako" $ screenChooser
    [ ( "Admin",  adminScreen),
      ( "Kupujacy", loginScreen "Kupujacy" logAsBuyer buyerScreen),
      ( "Dostawca", loginScreen "Dostawca" logAsProvider providerScreen),
      ( "Wlasciciel", loginScreen "Wlasciciel" logAsOwner ownerScreen) ]
  liftIO $ w

adminScreen = fromButtonList "Admin" $ screenChooser
  [ ("Dodaj uzytkownika", addUserScreen),
    ("Stworz baze", createDataBase),
    ("Zobacz kupujacych", listBuyerScreen),
    ("Zobacz wlascicieli", listOwnerScreen),
    ("Zobacz dostawcow", listProviderScreen),
    ("Usun histore", createDataBase) ]

listProviderScreen = do
  conn <- takeConn
  listScreen (doNothing) $ ((listAllProviders `unDB` conn) >>= splitHeader)
listOwnerScreen = do
  conn <- takeConn
  listScreen (doNothing) $ ((listAllOwners `unDB` conn) >>= splitHeader)
listBuyerScreen = do
  conn <- takeConn
  listScreen (doNothing) $ ((listAllBuyers `unDB` conn) >>= splitHeader)

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
    ( "Zobacz sprzedawcow", placeholder ),              -- > dodaj zamowienie
    ( "Zobacz niezlozone zamowienia", placeholder ) ]   -- > zloz zamowienie

buyerHistoryScreen = runInIO $ do
  conn <- takeConn
  rN <- takeRoInt
  tp <- takeTp 
  a <- listScreen (setMVarAndGoto tp buyerOrderDetailsScreen) $ ((buyerHistory rN `unDB` conn) >>= splitHeader)
  liftIO a

buyerOrderDetailsScreen = runInIO $ do
  conn <- takeConn
  kuid <- takeRoInt
  zaid <- takeTpInt
  a <- listScreen (doNothing) $ ((buyerOrderDetails kuid zaid `unDB` conn) >>= splitHeader)
  liftIO a

buyerProductScreen = runInIO $ do
  conn <- takeConn
  kuid <- takeRoInt
  tp <- takeTp 
  a <- listScreen (setMVarAndGoto tp buyerSeeOwnerOfScreen) $ ((buyerOptions kuid `unDB` conn) >>= splitHeader)
  liftIO a

buyerSeeOwnerOfScreen = runInIO $ do
  conn <- takeConn
  kuid <- takeRoInt
  tpid <- takeTpInt 
  pr <- takePr
  a <- listScreen (setMVarAndGoto pr buyerOrderScreen) $ ((whoHasX kuid tpid `unDB` conn) >>= splitHeader)
  liftIO a
  
buyerOrderScreen = runInIO $ do
  conn <- takeConn
  kuid <- takeRoInt
  tp <- takeTp 
  prid <- takePrInt
  a <- listScreen (setMVarAndGoto tp buyerAddToOrderScreen) $ ((buyerOrdersOfOwner kuid prid `unDB` conn) >>= splitHeader)
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
  

ownerScreen = fromButtonList "Wlasciciel" $ screenChooser
  [ ( "Historia zamowien", placeholder),                -- > szczegly zamowienia
    ( "Niezrealizowane zamowienia", placeholder),       -- > zrealizuj zamowienie
    ( "Zobacz swoje produkty", placeholder),
    ( "Dodaj produkt", placeholder),                    
    ( "Zobacz dostepne produkty", placeholder) ]        -- > kto je dostarcza -> dodaj produkt

providerScreen = fromButtonList "Dostawca" $ screenChooser
  [ ( "Wspolpracownicy", placeholder),
    ( "Co dostarczam", placeholder),                    -- > przestan dostarczac
    ( "Typy produktow", placeholder),                   -- > zacznij dostarczac
    ( "Dodaj nowy typ produktu", placeholder) ]
