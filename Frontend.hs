import Backend

import Graphics.Vty.Widgets.All
import Graphics.Vty.Attributes
import Graphics.Vty.LLInput

import qualified Data.Text as T
import Control.Monad
import Control.Monad.Reader
import Control.Concurrent.MVar
import Data.Time.LocalTime

type FrontEnd a = ReaderT (Collection, MVar [IO ()], MVar Int, MVar Int) IO a
runFrontEnd :: FrontEnd a -> IO ()
runFrontEnd act = do
  c <- newCollection
  m <- newMVar []
  i1 <- newMVar (-1)
  i2 <- newMVar (-1)
  a <- runReaderT act (c,m, i1, i2) 
  runUi c defaultContext

runInIO :: FrontEnd () -> FrontEnd (IO ())
runInIO act = do
  c <- ask
  return $ runReaderT act c 

fst4 (a,_,_,_) = a
snd4 (_,b,_,_) = b
thr4 (_,_,c,_) = c
fth4 (_,_,_,d) = d

addCollection :: Show a => Widget a -> Widget FocusGroup -> FrontEnd (IO ())
addCollection w wfg = do
  c <- ask
  a <- liftIO $ addToCollection (fst4 c) w wfg
  return $ (pushToStack (snd4 c) a >> a)

takeStack :: FrontEnd (MVar [IO ()])
takeStack = do
  c <- ask
  return $ snd4 c

takeIDU :: FrontEnd (MVar Int)
takeIDU = do
  c <- ask
  return $ thr4 c

takeIDZ :: FrontEnd (MVar Int)
takeIDZ = do
  c <- ask
  return $ fth4 c

takeCollection :: FrontEnd Collection
takeCollection = do
  c <- ask
  return $ fst4 c

pushInt :: MVar Int -> Int -> IO ()
pushInt mv i = modifyMVar_ mv (\a -> return i)

takeInt mv = modifyMVar mv (\a -> return (a,a))  

pushToStack :: MVar [IO ()] -> IO () -> IO ()
pushToStack mv act = modifyMVar_ mv (return . (act:))

popFromStack :: MVar [IO ()] -> IO (Maybe (IO ()))
popFromStack mv = modifyMVar mv (return .  safeUnCon)

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
  liftIO $ ( {-- boxFixed width height =<< --} centered txt)

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
  cw <- liftIO $ (centered =<< boxFixed width 1 =<< centered txt) 
    <--> (centered =<< bordered =<< boxFixed width 10 list) <--> (centered b)
  addWidgetToFG fg list
  addWidgetToFG fg b
  liftIO $ list `onItemActivated` (\(ActivateItemEvent _ a _) -> fs $ T.unpack a)
  addCollection cw fg
  

makeButton (str, f) = liftIO $ do
  b <- newButton $ T.pack str
  b `onButtonPressed` \_ -> f
  return $ buttonWidget b

addWidgetToFG fg w = liftIO $ addToFocusGroup fg w

listScreen :: FrontEnd (String -> IO ()) -> IO (String, [String]) -> FrontEnd (IO ())
listScreen ch act = do
  runInIO $ do
    (x,xs) <- liftIO $ act
    a <- fromButtonList x (ch >>= \f' -> return (f', xs))
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

main :: IO ()
main = runFrontEnd $ do
  w <- fromButtonList "Zaloguj sie jako" $ screenChooser
    [ ( "Admin",  placeholder),
      ( "Kupujacy", placeholder),
      ( "Dostawca", placeholder),
      ( "Wlasciciel", placeholder) ]
  liftIO $ w

