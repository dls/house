
import Gadgets--(posGo)
import ExtBadget(gadgetDisplay,displaySize)
--import Badget(gadgetDisplay)

--import Multicount
import Explode
--import Graph
--import Bristol
import Calc
import Updown
--import Escher
import TerminalTest
import DrawGIF
import GIF
import GIFparser
import System(getArgs)

main =
    do args <- getArgs
       gifs <- fmap (\rs->[(a,gif)|(a,Right gif)<-rs]) $
	       mapM (\a->fmap ((,) a . parseGIF) . readFile $ a) args
       posGo displaySize gadgetDisplay (demos gifs)
  where
    --demos = [counters,explode 4 4,gedit,bristol,calc,updown,escher]
    demos gifs = [--((10,10),terminaltest),
	     --((512,10),explode 4 4),
	     --((10,350),updown),((200,350),calc),
	     ((0,0),demowall gifs)]

demowall gifs = (mywall,"Gadgets Demo")
  where
    mywall =
      wire $ \w ->
      tx (op w) (Nothing,window (control (op w)) "Start") $
      wall' (width wi.height he.picture (colourbox (col "light grey"))) (ip w)

    (wi,he) = displaySize

    control o =
        setGapSize 5 $
        above ([b updown,b calc,b (explode 4 4),b terminal]
	       ++map (b.gifDisp) gifs)
      where
        b (g,title) =
          button' (width 80.height 20.picture (text fn co title))
		  o
		  (Nothing,window g title)
        fn = "fixed"
	co = col "black"

        terminal = terminaltest' (fontwmetrics "*-courier-medium-r-*-14-*" textSize)
          where
            textSize str = (9*length str,(12,3)) -- metrics for courier14

        gifDisp (path,gif) = (paper' changes,path)
          where
	    changes = width w . height h . picture (drawGIF pv gif) .
		      gridlock (10000,10000)

            GIF { screen_descriptor=SD{swidth=w,sheight=h}} = gif

            -- A hack for 24-bit graphics with blue in the lsb...
            pv (r,g,b) = fromIntegral ((r*256+g)*256+b)
{-
      tx (op w) (uncurry window updown) $
      tx (op w) (uncurry window calc) $
      tx (op w) (uncurry window (explode 4 4)) $
      tx (op w) (uncurry window terminaltest) $
-}

