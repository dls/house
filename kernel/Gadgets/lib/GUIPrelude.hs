module GUIPrelude(module GUIPrelude,module Wire,module Gio) where
import Prelude hiding (sequence)
--import IO(hPutStrLn,stderr)
import Control.Concurrent
--import qualified IOExts(trace)
import qualified Wire
import Wire(In(..),Out(..),Wire)
import Gio
--import ExtBadget(gadgetDisplay)
--import HOpBadget(gadgetDisplay)

-- *** Rob's graphical process bits *** --
 
--type Process a = a -> a

data ProcessEnv
  = PEnv {dispCmdCh :: Chan GCommand,
	  mouseCh :: Chan MouseEvnt,
	  keyboardCh :: Chan KeyboardEvnt,
	  screenCh :: Chan ScreenEvnt,
	  rd :: Wire.Reader,
          nextWire::MVar Int,
          myName::String}

newtype Process s = P { unP :: s -> ProcessEnv -> IO () }
--data In a = In Int
--data Out a = Out Int
type InOut a b = (In a,Out b)
type Duplex a b = (InOut a b,InOut b a)

infixr 0 ?
x ? y = \c -> x (\d -> y d c)
 
sequence :: [Process s -> Process s] -> Process s -> Process s
sequence [] c = c 
--sequence [f] c = f c
sequence (f:fs) c = f (sequence fs c)

accumulate :: [(b -> Process s) -> Process s] -> ([b] -> Process s) -> Process s
accumulate = accumulate' []
    where
    accumulate' a [] c = c (reverse a)
    accumulate' a (f:fs) c = f $ \v -> accumulate' (v:a) fs c

mapL [] _ = []
mapL _ [] = []
mapL (f:fs) (v:vs) = f v:mapL fs vs

ip :: Wire a -> In a
ip = fst

op :: Wire a -> Out a
op = snd

opFromIp :: In m -> Out m
opFromIp (In w) = Out w

ipFromOp :: Out m -> In m
ipFromOp (Out w) = In w

nci :: In a
nci = In (Wire.noWe "nci")

nco :: Out a
nco = Out (Wire.noWe "nco")

realOut (Out we) = Wire.realWire we
realIn (In we) = Wire.realWire we

--store fn s ~(Success:_) = [WriteFile fn s]


--primitive trace "primTrace" :: String -> a -> a
--trace = IOExts.trace

primLaunch :: GadgetDisplay -> Process s -> s -> IO ()
primLaunch gadgetDisplay (P p) s =
  do next <- newMVar 0
     r <- newChan
     cmdCh <- newChan
     evtCh <- newChan
     mouseCh <- newChan
     keyboardCh <- newChan
     screenCh <- newChan
     --putStrLn "forking gadgetDisplay"
     forkIO (gadgetDisplay cmdCh evtCh)
     forkIO (distributeEvents evtCh mouseCh keyboardCh screenCh)
     --putStrLn "starting first process"
     p s (PEnv cmdCh mouseCh keyboardCh screenCh r next "primLaunch")
  where
    distributeEvents evtCh mouseCh keyboardCh screenCh = distr
       where
         distr = do ev <- readChan evtCh
		    case ev of
		      ME me -> writeChan mouseCh me
		      KE ke -> writeChan keyboardCh ke
		      SE se -> writeChan screenCh se
		    distr


guiDevice name tag ch i o =
    myNameIs name $
    primSpawn (primClaim i $ copyCmds) () $
    copyEvnts
  where
    copyCmds = primRx [primFrom (const i) $ \ cmds -> txCmds cmds copyCmds]
                      copyCmds 
    copyEvnts = rxEvts $ \ evnt -> primTx o evnt $ copyEvnts

    txCmds = txDisplay . tag
    rxEvts = rxChan ch

mouse :: In MouseCmnd -> Out MouseEvnt -> Process s
mouse = guiDevice "mouse" MC mouseCh

screen :: In [ScreenCmnd] -> Out ScreenEvnt -> Process s
screen = guiDevice "screen" SC screenCh

txDisplay = txChan dispCmdCh

txChan ch x p = ioProcess' (flip writeChan x . ch) (const p)
rxChan ch   p = ioProcess' (readChan . ch) p

ioProcess' :: (ProcessEnv -> IO a) -> (a->Process s)->Process s
ioProcess' io p = P $ \ s e -> do x <- io e
				  unP (p x) s e

ioProcess io = ioProcess' (const io)

showit i =
    primClaim i $
    loop
  where
     loop = primRx [primFrom (const i) $ \ x -> typeout (show x) $ loop]
		   loop

keyboard :: In KeyboardCmnd -> Out KeyboardEvnt -> Process s
keyboard = guiDevice "keyboard" KC keyboardCh

data Guarded1 s = forall m . From (s->In m) (m->Process s)
type Guarded s = [Guarded1 s]

typeout :: String -> Process s -> Process s
typeout msg (P p) = P $ \ s e -> {-hPutStrLn stderr (myName e++": "++msg) >>-} p s e

primTx :: Out o -> o -> Process s -> Process s
primTx w o p =
    if realOut w
    then P $ \ s e ->  Wire.write w o >> unP p s e
    else p -- typeout "trying to write to non-wire" p

primRx :: Guarded s -> Process s -> Process s
primRx gds (P fail) =
  P $ \ s e ->
  let wgds = [Wire.from w (cont c)|From si c<-gds, let w=si s,realIn w]
      cont c i = unP (c i) s e
  in Wire.receive (rd e) wgds (fail s e)

primFrom :: (s -> In m) -> (m -> Process s) -> Guarded1 s
primFrom = From

primTerminate :: Process s
primTerminate = P $ \ s e -> return ()

myNameIs :: String -> Process s -> Process s
myNameIs me (P p) = P $ \ s e -> p s e{myName=me}

primClaim :: In m -> Process s -> Process s
primClaim i (P p) | realIn i =
   P $ \ s e -> do Wire.claim i (rd e)
		   --putStrLn $ myName e++" claims "++show i
		   p s e
primClaim i p = p -- typeout "claiming non-wire" p

primDisown :: In m -> Process s -> Process s
primDisown i (P p) =  P $ \ s e -> Wire.disown i >> p s e

primWire :: (Wire a -> Process s) -> Process s
primWire wp = P $ \ s e -> do i <- modifyMVar (nextWire e) (\i->return (i+1,i))
			      w <- Wire.new i
                              unP (wp w) s e

primWires :: Int -> ([Wire a] -> Process s) -> Process s
primWires n = accumulate (replicate n primWire)

primSpawn :: Process a -> a -> Process s -> Process s
primSpawn (P p1) s1 (P p2) =
  P $ \ s2 e -> let new = do r <- newChan
			     p1 s1 e{rd=r}
			     return ()
		in forkIO new >> p2 s2 e

{-
#define undefined (error ("undefined at "++__FILE__ ++ " "++show __LINE__))
-}

showGraph :: a -> Process s -> Process s
showGraph = undefined

{-# NOINLINE textSize #-}
textSize  :: String -> String -> (Int,(Int,Int))
textSize fn str = (6*length str,(11,2)) -- not implemenet yet!
--textSize fn str = (9*length str,(12,3)) -- hardwired metrics for courier14
