module ExtBadget(gadgetDisplay,displaySize) where
import qualified Gio
import IO
import Control.Concurrent
default(Int)

displaySize = (1000,700)

gadgetDisplay :: Gio.GadgetDisplay
gadgetDisplay cmdCh evCh  =
    do hSetBuffering stdout LineBuffering
       hSetBuffering stdin LineBuffering
       forkIO $ loop $ doGCommand =<< readChan cmdCh
       loop $ writeChan evCh =<< getGEvent
  where loop io = io >> loop io

doGCommand :: Gio.GCommand -> IO ()
doGCommand = print

getGEvent :: IO Gio.GEvent
getGEvent = readLn
