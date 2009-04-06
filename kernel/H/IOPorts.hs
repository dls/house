{-|
Programmed IO (section 3.4 in the paper)

Many PC devices (including timers, the CMOS and real-time clock
devices, the keyboard and mouse, network interfaces, etc.)  are
controlled by reading and writing data and control information to
specified /ports/ via special @in@ and @out@ instructions.  These are
available through 'H' via the functions exported from this module.
-}
module H.IOPorts(H,Port,inB,inW,inL,outB,outW,outL) where
import H.Monad(H,liftIO)
import Data.Word



---------------------INTERFACE----------------------------------

type Port = Word16

inB :: Port -> H Word8
inW :: Port -> H Word16
inL :: Port -> H Word32

outB :: Port -> Word8 -> H()
outW :: Port -> Word16 -> H()
outL :: Port -> Word32 -> H()

-------------PRIVATE IMPLEMENTATION FOLLOWS---------------------

inB = liftIO . inBx
inW = liftIO . inWx
inL = liftIO . inLx

outB p = liftIO . outBx p
outW p = liftIO . outWx p
outL p = liftIO . outLx p


foreign import ccall unsafe "io.h inb" inBx :: Port -> IO Word8
foreign import ccall unsafe "io.h inw" inWx :: Port -> IO Word16
foreign import ccall unsafe "io.h inl" inLx :: Port -> IO Word32

foreign import ccall unsafe "io.h outb" outBx :: Port -> Word8 -> IO ()
foreign import ccall unsafe "io.h outw" outWx :: Port -> Word16 -> IO ()
foreign import ccall unsafe "io.h outl" outLx :: Port -> Word32 -> IO ()
