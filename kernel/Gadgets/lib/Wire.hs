module Wire(Reader,Wire,WireId,wireId,In(..),Out(..),noWe,realWire,new,write,claim,disown,Guarded,from,receive) where
--import IO(hPutStrLn,stderr)
import Control.Concurrent
import Control.Monad(unless)

{-
#define undefined (error ("undefined at "++__FILE__ ++ " "++show __LINE__))
-}

type Reader = Chan WireId
data WireEnd a
   = WE { oldWireId::Int,
          optReader::MVar (Either Int Reader),
          wireBuf::Chan a}
--   | NoWE

newtype WireId = WireId (MVar (Either Int Reader)) deriving (Eq)
wireId = WireId . optReader

noWe s = WE (-1) (error $ "no reader "++s) (error $ "no wirebuf "++s)
--noWe = NoWE
realWire we = oldWireId we>=0

sameWE we1 we2 = wireId we1==wireId we2
instance Eq (WireEnd a) where (==) = sameWE
instance Show (WireEnd e) where show we = "Wire "++show (oldWireId we)

newtype In a = In (WireEnd a) deriving (Eq,Show)
newtype Out a = Out (WireEnd a) deriving (Eq,Show)
type Wire a = (In a,Out a) --Wire Int
--sameWire (i1,o1) (i2,o2) = sameWE i1 i2 && sameWE o1 o2

new n = do optr <- newMVar (Left 0)
	   buf  <- newChan
	   let we = WE n optr buf
	   --putStrLn $ "Wire.new "++show n
	   return (In we,Out we)

write (Out we@(WE _ optr m)) x =
    do writeChan m x
       r <- takeMVar optr
       case r of
         Left cnt  -> putMVar optr (Left (cnt+1))
	 Right rdr -> do writeChan rdr (wireId we)
			 putMVar optr r

claim (In we@(WE n optr m)) rdr =
    do r <- takeMVar optr
       case r of
         Right _  -> do putMVar optr r
		        fail ("reclaiming wire "++show n)
	 Left cnt -> do sequence_ (replicate cnt (writeChan rdr (wireId we)))
			putMVar optr (Right rdr)

disown (In (WE n optr m)) = either err disown' =<< readMVar optr
  where
     disown' _ = swapMVar optr (Left 0) >> return () -- !!! lost msg count
     err _ = fail ("disown unclaimed wire "++show n)

data Guarded r = forall a . From (In a) (a->IO r)
from = From

receive :: Reader -> [Guarded r] -> IO r -> IO r
receive r gs unclaimed =
  do n <- readChan r
     case [c=<<read w|From (In w) c<-gs,wireId w==n] of
       [] -> unclaimed
       c:_ -> c
  where
    read = readChan . wireBuf
