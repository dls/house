module TerminalTest where
import Gadgets hiding (Maybe(..))
import Control.Concurrent(Chan)

terminaltest = terminaltest' id

terminaltest' params = (tt' params,"Terminal")
--tt :: Gadget
tt' change =
    wire $ \ to ->
    wire $ \ from ->
    spawn (echo (ip from) (op to)) $
    terminal' change (ip to) (Just (op from))
  where
    echo i o = claim i loop :: Component
      where
	loop = rx [from i $ \ c -> tx o (conv c) loop] (rxFail "echo")
	conv '\r' = "\n"
	conv c = [c]


terminaltest2 title outCh optInCh = (tt2 outCh optInCh,title)
tt2 :: Chan String -> Maybe (Chan Char) -> Gadget
tt2 = tt2' id
tt2' change outCh optInCh =
    wire $ \ to ->
    spawn (echoFromChan outCh (op to)) $
    case optInCh of
      Nothing -> terminal' (size tsize.change) (ip to) Nothing
      Just inCh ->
        wire $ \ from ->
        spawn (echoToChan (ip from) inCh) $
        terminal' (size tsize.change) (ip to) (Just (op from))
  where
    echoFromChan ch o = loop :: Component
      where
	loop = rxChan (const ch) $ \ s -> tx o (map conv s) loop

    conv '\r' = '\n'
    conv c = c

    echoToChan o ch = claim o loop :: Component
      where
	loop = rx [from o $ \ c -> txChan (const ch) (conv c) loop]
	          (rxFail "echoToChan")

    tsize = (w,h)
    w = min 860 (80*cw) `div` cw -- 80 columns or 540 pixels, whichever is less 
    h = min 600 (24*ch) `div` ch -- 24 lines or 600 pixels, whichever is less 
    --h = min 750 (48*ch) `div` ch -- 48 lines or 750 pixels, whichever is less 
    (cw,(a,d)) = textSize "X"
    ch = a+d

    textSize = ts (change defaults)
