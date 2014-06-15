module Frontend.Admin where 

import Frontend.Common
import DBTransaction

import Graphics.Vty.Widgets.All
import Graphics.Vty.Attributes
import Graphics.Vty.LLInput

import qualified Data.Text as T
import Control.Monad
import Control.Monad.Reader (liftIO, lift)
import Data.Maybe (fromJust)

import Backend.Admin

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
