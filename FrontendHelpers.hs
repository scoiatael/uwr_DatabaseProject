module FrontendHelpers where

import DBTransaction

import Database.HDBC.Types (SqlError, seErrorMsg)
import Database.HDBC.PostgreSQL (Connection)

import Graphics.Vty.Widgets.All
import Graphics.Vty.Attributes
import Graphics.Vty.LLInput

import qualified Data.Text as T
import Control.Monad
import Control.Monad.Reader
import Control.Concurrent.MVar
import Data.Time.LocalTime

import Control.Exception.Base

import Text.Read (readsPrec)
intFromString :: String -> Maybe Int
intFromString str = case (readsPrec 0 str) :: [(Int, String)] of
  [] -> Nothing
  (i,_):_ -> Just i
  
floatFromString :: String -> Maybe Double
floatFromString str = case (readsPrec 0 str) :: [(Double, String)] of
  [] -> Nothing
  (i,_):_ -> Just i
  
catchSql :: IO a -> (SqlError -> IO a) -> IO a
catchSql = catch

inThis = flip catchSql
goodByeScreen :: Collection -> SqlError -> IO ()
goodByeScreen c err = do
  wfg <- newFocusGroup
  let t = "Error occured:\n" ++ seErrorMsg err
  let width = length t
  let height = length $ lines t
  bW <- newButton $ T.pack "Quit"
  b <- hCentered $ buttonWidget bW
  bF <- wfg `addToFocusGroup` b  
  wText  <- centered =<< boxFixed width height =<< centered =<< plainText (T.pack t)
  w <- wText `vBox` b
  wfg `onKeyPressed` \_ k _ ->
    error "Quit"
  a <- addToCollection c w wfg
  a
    
type FrontEnd a = ReaderT (Collection, MVar [IO ()], MVar Int, MVar Int, MVar Int) DBTransaction a
runFrontEnd :: FrontEnd a -> IO ()
runFrontEnd act = do
  c <- newCollection
  m <- newMVar []
  i1 <- newMVar (-1)
  i2 <- newMVar (-1)
  i3 <- newMVar (-1)
  (flip catchSql) (goodByeScreen c) $ void $ runTransaction $ runReaderT act (c,m, i1, i2, i3) 
  runUi c defaultContext

runInIO :: FrontEnd () -> FrontEnd (IO ())
runInIO act = do
  c <- ask
  err <- errScreen
  a <- lift $ runInTrans $ runReaderT act c 
  return $ a `catchSql` err

takeConn :: FrontEnd Connection
takeConn = lift conn

errScreen :: FrontEnd ( SqlError -> IO () )
errScreen = do
  b <- backButton
  fg <- simpleFocusGroup 
  fg `addWidgetToFG` b
  cb <- liftIO $ centered b
  all <- ask
  let c = fst5 all
  return $ \err -> do
    let t = "Error occured:\n" ++ seErrorMsg err
    let width = length t
    let height = length $ lines t
    wText  <- centered =<< boxFixed width height =<< centered =<< plainText (T.pack t)
    w <- wText `vBox` cb
    a <- addToCollection c w fg
    a

fst5 (a,_,_,_,_) = a
snd5 (_,b,_,_,_) = b
thr5 (_,_,c,_,_) = c
frt5 (_,_,_,d,_) = d
fft5 (_,_,_,_,e) = e

addCollection :: Show a => Widget a -> Widget FocusGroup -> FrontEnd (IO ())
addCollection w wfg = do
  c <- ask
  a <- liftIO $ addToCollection (fst5 c) w wfg
  return $ (pushToStack (snd5 c) a >> a)

takeStack :: FrontEnd (MVar [IO ()])
takeStack = do
  c <- ask
  return $ snd5 c

takeTp :: FrontEnd (MVar Int)
takeTp = do
  c <- ask
  return $ thr5 c
  return $ frt5 c
takeTpInt = takeTp >>= (\m -> liftIO $ takeInt m)

takeRo :: FrontEnd (MVar Int)
takeRo = do
  c <- ask
  return $ frt5 c
takeRoInt = takeRo >>= (\m -> liftIO $ takeInt m)

takePr :: FrontEnd (MVar Int)
takePr = do
  c <- ask
  return $ fft5 c
takePrInt = takePr >>= (\m -> liftIO $ takeInt m)

takeCollection :: FrontEnd Collection
takeCollection = do
  c <- ask
  return $ fst5 c

pushInt :: MVar Int -> Int -> IO ()
pushInt mv i = modifyMVar_ mv (\a -> return i)

takeInt mv = modifyMVar mv (\a -> return (a,a))  

pushToStack :: MVar [IO ()] -> IO () -> IO ()
pushToStack mv act = modifyMVar_ mv (return . (act:))

popFromStack :: MVar [IO ()] -> IO (Maybe (IO ()))
popFromStack mv = modifyMVar mv (return .  safeUnCon)

makeButton (str, f) = liftIO $ do
  b <- newButton $ T.pack str
  b `onButtonPressed` \_ -> f
  return $ buttonWidget b

safeUnCon :: [a] -> ([a], Maybe a)
safeUnCon [] = ([], Nothing)
safeUnCon [_] = ([], Nothing)
safeUnCon (x:(y:xs)) = ((y:xs), Just y)

safeBack :: Maybe (IO ()) -> IO ()
safeBack Nothing = error "Quit"
safeBack (Just f) = f

backButton = do
  mv <- takeStack
  makeButton ("Back", popFromStack mv >>= safeBack)

simpleFocusGroup = do
  fg <- liftIO $ newFocusGroup
  liftIO $ fg `onKeyPressed` \_ k _ ->
    if k == KEsc then error "Quit" else return False
  return fg

simpleText t = do
  let width = length t
  let height = length $ lines t
  txt <- liftIO $ plainText $ T.pack t
  liftIO $ ( boxFixed width height =<< centered txt)

infoScreen :: String -> FrontEnd (IO ())
infoScreen t = do
  fg <- simpleFocusGroup
  b <- backButton
  txt <- simpleText t
  let cb = (centered b)
  let ct = (centered txt)
  cw <- liftIO $ ct <--> cb
  addWidgetToFG fg b
  addCollection cw fg

fromButtonList :: String -> FrontEnd (String -> IO (), [String]) -> FrontEnd (IO ()) 
fromButtonList t f = do
  (fs,ls) <- f
  list <- liftIO $ newTextList current_attr (map T.pack ls) 1
  fg <- liftIO $ newFocusGroup
  liftIO $ fg `onKeyPressed` \_ k _ -> do
    if k == KEsc then error "Quit" else return False
  txt <- liftIO $ plainText $ T.pack t
  b <- backButton
  let width = maximum $ map (length) $ t:(ls)
  cw <- liftIO $ (centered =<< boxFixed width 2 =<< centered txt) 
    <--> (centered =<< bordered =<< boxFixed width 10 list) <--> (centered b)
  addWidgetToFG fg list
  addWidgetToFG fg b
  liftIO $ list `onItemActivated` (\(ActivateItemEvent _ a _) -> fs $ T.unpack a)
  addCollection cw fg
  

addWidgetToFG fg w = liftIO $ addToFocusGroup fg w

listScreen :: String -> FrontEnd (String -> IO ()) -> IO (String, [String]) -> FrontEnd (IO ())
listScreen title ch act = do
  runInIO $ do
    (x,xs) <- liftIO $ act
    a <- fromButtonList (title ++ "\n"++ x) (ch >>= \f' -> return (f', xs))
    liftIO a

doNothing :: FrontEnd (String -> IO ())
doNothing = return $ (\_ -> return ())

setMVarAndGoto mv act = do
  f <- act 
  return $ \str -> let i = read (words str !! 0) :: Int in do
    pushInt mv i 
    f

splitHeader :: [String] -> IO (String, [String])
splitHeader (x:xs) = return (x, xs)
addHeader :: (a -> String) -> String -> [a] -> IO (String, [String])
addHeader f s strs = return (s, map f strs)

addText t w = liftIO $ do
  txt <- simpleText t
  (centered txt) <++> (centered w)

createEditLineAddFG fg = do
  e <- liftIO $ editWidget
  addWidgetToFG fg e
  return e

createEditMultiLineAddFG fg = do
  e <- liftIO $ multiLineEditWidget
  addWidgetToFG fg e
  return e

blank act = act >>= \f -> return $ \_ -> f

placeholder :: FrontEnd (IO ())
placeholder = return $ return ()


chooser :: [(String, IO ())] -> String -> IO ()
chooser [] s = error "No such item"
chooser ((s,f):l) s' = if s == s' then f else chooser l s'

screenChooser :: [(String, FrontEnd (IO ()))] -> FrontEnd (String -> IO (), [String])
screenChooser list = do
  let strs = map fst list
  fs <- sequence $  map snd list
  return $ (chooser $ zip strs fs, strs)

confirmationScreen title answer action = runInIO $ do
  conn <- takeConn
  kuid <- takeRo
  zaid <- takeTp
  prid <- takePr 
  st <- takeStack
  conn <- takeConn
 
  nextScreen <- infoScreen answer 
  catchErr <- errScreen
  ok <- makeButton ("Ok", catchErr `inThis` (action kuid zaid prid conn st >> nextScreen))
  b <- backButton
  tytul <- simpleText title
  fg <- simpleFocusGroup
  addWidgetToFG fg ok
  addWidgetToFG fg b
     
  cw <- liftIO $ (centered tytul) <--> (centered ok) <--> (centered b)
  a <- addCollection cw fg
  liftIO a 
