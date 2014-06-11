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
      ( "Kupujacy", loginScreen logAsBuyer buyerScreen),
      ( "Dostawca", loginScreen logAsProvider providerScreen),
      ( "Wlasciciel", loginScreen logAsOwner ownerScreen) ]
  liftIO $ w

adminScreen = fromButtonList "Admin" $ screenChooser
  [ ("Dodaj uzytkownika", placeholder),
    ("Stworz baze", createDataBase),
    ("Usun histore", createDataBase) ]

loginScreen log next = do
  fg <- simpleFocusGroup 
  back <- backButton
  name <- createEditLineAddFG fg
  conn <- takeConn
  nextScreen <- next
  onErr <- errScreen
  errScreen <- infoScreen "ID powinno byc liczba!"
  ok <- makeButton ("Ok",
    do
      txt <- liftM T.unpack $ getEditText name
      let n = intFromString txt
      if n == Nothing 
        then errScreen
        else do
          (log (fromJust n) "" `unDB` conn) `catchSql` onErr
          nextScreen
    )
  tytul <- simpleText "Logowanie"
  addWidgetToFG fg ok
  addWidgetToFG fg back
  nGr <- addText "ID:" name
  cw <- liftIO $ (centered tytul) <--> (centered nGr) <--> (centered ok) <--> (centered back)
  addCollection cw fg
  
buyerScreen = placeholder
ownerScreen = placeholder
providerScreen = placeholder

createDataBase = runInIO $ do
  lift $ resetDatabase
  a <- infoScreen "Zrobione."
  liftIO a
