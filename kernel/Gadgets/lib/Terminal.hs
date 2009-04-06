module Terminal(terminal,terminal',Terminal(..),defaults) where
import Prelude --hiding (Maybe(..))
import Data.Maybe(isJust)
import GadgetsPrelude hiding (Maybe(..))
import Components

data Terminal
  = Terminal { fg,bg::Colour,
               columns,rows::Int,
	       fn::String,
	       ts::String -> (Int,(Int,Int))
	     }

instance HasFGColour Terminal where fgcol c p = p{fg=c}
instance HasBGColour Terminal where bgcol c p = p{bg=c}
instance HasFont     Terminal where fontwmetrics f m p = p{fn=f,ts=m}
instance HasSize     Terminal where size (c,r) p = p{columns=c,rows=r}

defaults = Terminal { fg = col "black",
		      bg = col "paper",
		      columns = 80,
		      rows = 24,
		      fn = "fixed",
		      ts = textSize "fixed" }

-- Representaiton of the text in the terminal window:
type Text = (String,[Line]) -- (current line,previous lines)
type Line = (String,Bool) -- (contents,terminated or wrapped?)
text0 = ("",[]) :: Text

terminal :: In String -> Maybe (Out Char) -> Gadget
terminal = terminal' id

terminal' change i opto =
    initGadget (columns params*chwidth,rows params*lnheight) blankbg $
    claim i $
    (if isJust opto then txSM SMClaimFocus else id) $
    tm True text0
  where
    params = change defaults

    tm :: Bool -> Text -> Gadget
    tm focus txt =
	rx [
	    from i $ \ s ->
	      let (needredraw,_,txt1) = append (columns params) txt s
	          txt' = taketxt (rows params) txt1
		  p = textPos txt
		  p' = textPos txt'
		  redraw = drawText focus p' txt'
		  optupdate = drawNewText focus p p' `fmap`
                                wrap (columns params) (hpos txt) s
	          scroll = txtlength txt1>rows params -- scrolling required?
	      in case (optupdate,needredraw || scroll) of
	           (Just update,False) ->
	                txSM (SMUpdateDrawFun update redraw) $ newtxt txt'
	           _ -> txSM (SMDrawFun redraw) $ newtxt txt',
	    fromSM $ \r ->
              case (r,opto) of
                (SMKeyPress c,Just o) -> tx o c $ same
	        (SMMouseClick{},Just o) ->
	            if focus
		    then same
		    else txSM SMClaimFocus $ changeFocus True
	        (SMLoseFocus,Just o) -> changeFocus False
		_ -> same
	] (rxFail "tm")
      where
        same = tm focus txt
	newtxt = tm focus
	changeFocus focus' =
            txSM (SMUpdateDrawFun update redraw) $ tm focus' txt
          where
	    p = textPos txt
	    update _ _ = showCursor focus' p
	    redraw = drawText focus' p txt

    drawText focus p (l,ls) size area =
	blankbg size area++
	optCursor++
	DrawSetFont (fn params):
	DrawSetColour (fg params):
	zipWith drawLine [ascent,ascent+lnheight..] ss
      where
	ss = reverse (l:map fst ls)
	optCursor = if focus then cursorOn p else []

    drawNewText focus p@(x,y) p' ls size area =
	cursorOff p++cursorOn p'++
	DrawSetFont (fn params):
	DrawSetColour (fg params):
	zipWith DrawText ps ls
      where
	ps = zip (x:repeat 0) [y+ascent,y+ascent+lnheight..]

    textPos (l,ls) = (fst (ts params l),lnheight*length ls)

    (chwidth,(ascent,descent)) = ts params "X"
    lnheight=ascent+descent

    showCursor focus = if focus then cursorOn else cursorOff
    cursorOn = cursor "red"
    cursorOff = cursor "paper"

    cursor colorname p@(x,y) =
	[DrawSetColour (col colorname),
	 DrawFilledRectangle (p,(x+chwidth-1,y+lnheight-1))]

    blankbg = colourbox (bg params)

txtlength (l,ls) = 1+length ls
taketxt n (x,xs) = (x,take (n-1) xs)
hpos (l,ls) = length l

append columns txt = foldl append1 (False,hpos txt,txt)
  where
    append1 (r,x,txt@(l,ls)) c =
      case c of
        '\n' -> case txt of
		  ("",(s,False):ls) -> (r,x,("",(s,True):ls))
	          _ -> (r,0,("",(l,True):ls))
        '\f' -> (True,0,text0) -- clear screen, redrawing required
        '\b' -> case txt of
		  (l@(_:_),ls) -> (r,x-1,(init l,ls))
		  ("",(s,terminated):ls) ->
		      if terminated
		      then if length s==columns
		           then (r,x,("",(s,False):ls))
			   else next
		      else append1 next c
                    where txt' = (s,ls); next = (r,hpos txt',txt')
		  _ -> (r,x,txt)
        _    -> if x+1<columns
		then (r,x+1,(l++[c],ls))
		else (r,0,([],(l++[c],False):ls))

wrap columns x s =
    if '\f' `elem` s
    then Nothing
    else Just $ concat $ zipWith wrapline xs (lines (bs "" s))
  where
    xs = columns-x:repeat columns

    bs acc [] = reverse acc
    bs acc ('\b':s) = bs (drop 1 acc) s
    bs acc (c:s) = bs (c:acc) s

    wrapline n "" = [""]
    wrapline n s = wrapline' n s
    wrapline' n "" = []
    wrapline' n s = case splitAt n s of
		      (s1,s2) -> s1:wrapline' columns s2

drawLine y s = DrawText (0,y) s
