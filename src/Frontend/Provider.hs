module Frontend.Provider where 

import Frontend.Common
import DBTransaction

import Graphics.Vty.Widgets.All
import Graphics.Vty.Attributes
import Graphics.Vty.LLInput

import qualified Data.Text as T
import Control.Monad
import Control.Monad.Reader (liftIO, lift)
import Data.Maybe (fromJust)

import Backend.Provider

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
