module HOpBadget(gadgetDisplay,screenSize,findFont) where
import Gio
import Control.Monad.State
import Control.Monad.Trans
import Data.Maybe(listToMaybe,fromMaybe)
import Control.Monad(mplus)
import Data.List(sortBy)
import Data.Char(isDigit,isLower,isAscii)
import Data.Set(toList)
import Kernel.Driver.IA32.VbeGraphics
import Kernel.Driver.Keyboard as K
import Kernel.Driver.Mouse as M
import Kernel.Driver.PS2
import H.Concurrency
import H.Monad
--import Font(Fonts)

--gadgetDisplay :: Graphics -> Fonts -> GadgetDisplay
gadgetDisplay gfx fonts cmdCh evCh  =
    do --hSetBuffering stdout LineBuffering
       --hSetBuffering stdin LineBuffering
       forkH $ loop $
                flip evalStateT (defaultFont fonts) $
	          doGCommand gfx fonts =<< liftH (readChan cmdCh)
       let size@(w,h) = screenSize gfx
       liftH $ writeChan evCh (SE (Expose ((0,0),(w-1,h-1))))
       k <- kbdChan
       m <- mouseChan size
       forkH $ loop $ getMouse gfx m evCh
       loop $ getKey k evCh
  where
    loop m = m >> loop m

defaultFont fonts = snd (head fonts)

--doGCommand :: Gio.GCommand -> IO ()
doGCommand gfx fonts gcmd =
  case gcmd of
    SC scmds -> do liftH $ hideCursor gfx
		   mapM_ (doScreenCmnd gfx fonts) scmds
		   liftH $ showCursor gfx
--  MC MouseCmnd
--  KC KeyboardCmnd
    _ -> return ()

doScreenCmnd gfx fonts scmd =
  case scmd of
    DrawSetPixel p c -> liftH $ setPixelValue gfx c >> setPixel gfx p
    DrawLine (p1,p2) -> liftH $ drawLine gfx p1 p2
    DrawFilledRectangle ((x1,y1),(x2,y2)) ->
        liftH $ fillRectangle gfx x0 y0 w h
      where x0 = min x1 x2
	    y0 = min y1 y2
	    w = abs (x1-x2)+1
	    h = abs (y1-y2)+1
    DrawPixmap (Pixmap cmap transp size pxls) p -> liftH $ drawPixmap gfx p size cmap transp pxls
    DrawSetPixelValue pxl -> liftH $ setPixelValue gfx pxl
    DrawSetColour rgb -> liftH $ setColor gfx rgb
    DrawSetFont fn -> put (findFont fonts fn)
    DrawCircle ((x,y),r) -> liftH $ drawCircle gfx x y r
    DrawSetClip ((l,t),(r,b)) -> liftH $ setClipRect gfx l t r b
    DrawText (x,y) s -> do font <- get
                           liftH $ drawText gfx font x y s
    DrawFilledTriangle (p1,p2,p3) ->
       -- An unfilled triangle is better than no triangle at all
       liftH $ drawLines gfx [p1,p2,p3,p1]
    _ -> return ()

--getGEvent :: IO Gio.GEvent
--getGEvent = return ()

kbdChan = launchKeyboardInterpreter =<<
	  launchKeyboardDecoder =<<
	  launchKeyboardDriver

mouseChan size = launchMouseDecoder size =<< launchMouseDriver

getKey kbdCh evCh =
  do K.KeyPress mods key <- readChan kbdCh
     case key of
       Key c -> wr (control mods c)
       BackspaceKey -> wr '\b'
       ReturnKey -> wr '\r'
       _ -> return ()
  where
    wr c = writeChan evCh (KE (Gio.KeyPress c))
    control mods c = if isAscii c && isLower c && hasControl mods
                     then toEnum (fromEnum c - 96)
		     else c
    hasControl = not . null . filter ctrl . toList
    ctrl (Ctrl _) = True
    ctrl _ = False

getMouse gfx mouseCh evCh = mouseEvent =<< readChan mouseCh
  where
    mouseEvent m =
       case m of
	 Up   b x y -> wr (MouseUnClick x y (button b))
	 Down b x y -> wr (MouseClick   x y (button b))
	 MoveTo x y -> moveCursor gfx x y -- >> wr (MouseMove x y)

    wr = writeChan evCh . ME
{-
    updMousePointer m =
       case m of
	 MoveTo x y -> setMousePointer x y
	 _ -> return ()
-}
    button M.Left = 1
    button M.Right = 3
    button M.Middle = 2

findFont fonts = convName
  where
    convName = conv . dropwild . map swapdashes . words . swapdashes

    swapdashes = map swapdash

    swapdash ' ' = '-'
    swapdash '-' = ' '
    swapdash c   = c

    dropwild ("*":ws) = ws
    dropwild ws = ws

    conv (fn:ws) = conv2 fn ws

    conv2 fn ("bold":ws) = conv3 fn "bold" ws
    conv2 fn ws = conv3 fn "" (dropwild ws)

    conv3 fn b ws = 
      case dropWhile (not . all isDigit) ws of
        w:ws -> conv4 fn b (convsize (read w::Int))
	[] -> conv4 fn b 13

    convsize n = if n<80 then n else n `div` 10

    conv4 fn b s = 
        fromMaybe (defaultFont fonts) $
        lookup (fn++b) fs `mplus`
	lookup fn fs `mplus`
	fmap snd (listToMaybe fs)
      where
	fs = mapFst fst (sortBy cmpsize fonts)
	cmpsize ((_,fs1),_) ((_,fs2),_) = compare (abs (fs1-s)) (abs (fs2-s))

mapFst = map . apFst
apFst f (x,y) = (f x,y)
