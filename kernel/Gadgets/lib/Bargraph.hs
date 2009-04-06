module Bargraph where
import Prelude hiding (sequence)
import GadgetsPrelude
import Components

data BarGraphAttributes
    = BarGraphAttributes Int Int Int Int Colour Colour
 
instance HasWidth BarGraphAttributes where
    width w (BarGraphAttributes _ h s b fgc bgc) = (BarGraphAttributes w h s b fgc bgc)
instance HasHeight BarGraphAttributes where
    height h (BarGraphAttributes w _ s b fgc bgc) = (BarGraphAttributes w h s b fgc bgc)
instance HasNumber BarGraphAttributes where
    number s (BarGraphAttributes w h _ b fgc bgc) = (BarGraphAttributes w h s b fgc bgc)
instance HasBorder BarGraphAttributes where
    border b (BarGraphAttributes w h s _ fgc bgc) = (BarGraphAttributes w h s b fgc bgc)
instance HasFGColour BarGraphAttributes where
    fgcol fgc (BarGraphAttributes w h s b _ bgc) = (BarGraphAttributes w h s b fgc bgc)
instance HasBGColour BarGraphAttributes where
    bgcol bgc (BarGraphAttributes w h s b fgc _) = (BarGraphAttributes w h s b fgc bgc)
 
bargraph = bargraph' id

bargraph' :: Change BarGraphAttributes -> [In (Int->Int)] -> Gadget
bargraph' cba is =
    let (BarGraphAttributes sx sy d g fgc bgc) = 
	    cba (BarGraphAttributes 50 101 10 5 (col "red") (col "black"))
	i = map (\p -> (p,())) is
	b l =
	    rx [
		froms i $ \fn _ ->
		    let l' = fn l in
		    txSM (SMDrawFun (level l')) $
		    b l',
		fromSM $ \sm ->
		    b l
	    ] (rxFail "bargraph")
	(fgc',bgc',flc,shc,lic,hic,foc,enc,dic,c1,c2,c3,c4,c5) = colourset
	level :: Int -> DrawFun
	level n (x',y') _ = 
	    let x = x'-g-1
		y = y'-g-1
		ys = y `div` d
	    in
	    plinth g shc lic dic (0,0) (x'-1,y'-1)++
	    [DrawSetColour bgc, fillbox ((g,g),(x,y)),
	    DrawSetColour fgc]++
	    [fillbox ((g+1,y-(yp*(y-g)) `div` d+1),(x-1,y-(yp*(y-g)) `div` d+ys-1)) | yp <- [1..n]]++
	    [DrawSetColour (mixCol (mixCol fgc bgc) bgc)]++
	    [fillbox ((g+1,y-(yp*(y-g)) `div` d+1),(x-1,y-(yp*(y-g)) `div` d+ys-1)) | yp <- [n+1..d]]
    in
    initGadget (sx,sy) (level 0) $
    myNameIs "bargraph" $
    sequence (map claim is) $
    b 0
