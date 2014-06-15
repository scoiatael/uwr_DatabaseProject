module Frontend.Main where

import Frontend.Common
import Frontend.Admin
import Frontend.Provider
import Frontend.Buyer
import Frontend.Owner
import Backend.Common
import DBTransaction

import Graphics.Vty.Widgets.All
import Graphics.Vty.Attributes
import Graphics.Vty.LLInput

import qualified Data.Text as T
import Control.Monad
import Control.Monad.Reader (liftIO, lift)

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

