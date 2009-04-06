module Paper where
import GadgetsPrelude hiding (Maybe(..))
import Components
import Useful
default(Int,Double)

data PaperAttributes
    = PaperAttributes Int Int (Int,Int) DrawFun (In DrawFun) (Out DrawFun) Colour (Maybe Colour) Int
 
instance HasWidth PaperAttributes where
    width w (PaperAttributes _ h g d di do' fgc bgc b) = (PaperAttributes w h g d di do' fgc bgc b)
instance HasHeight PaperAttributes where
    height h (PaperAttributes w _ g d di do' fgc bgc b) = (PaperAttributes w h g d di do' fgc bgc b)
instance HasGridLock PaperAttributes where
    gridlock g (PaperAttributes w h _ d di do' fgc bgc b) = (PaperAttributes w h g d di do' fgc bgc b)
instance HasPicture PaperAttributes where
    picture d (PaperAttributes w h g _ di do' fgc bgc b) = (PaperAttributes w h g d di do' fgc bgc b)
instance HasPictureIn PaperAttributes where
    pictureIn di (PaperAttributes w h g d _ do' fgc bgc b) = (PaperAttributes w h g d di do' fgc bgc b)
instance HasPictureOut PaperAttributes where
    pictureOut do' (PaperAttributes w h g d di _ fgc bgc b) = (PaperAttributes w h g d di do' fgc bgc b)
instance HasFGColour PaperAttributes where
    fgcol fgc (PaperAttributes w h g d di do' _ bgc b) = (PaperAttributes w h g d di do' fgc bgc b)
instance HasBGColour PaperAttributes where
    bgcol bgc (PaperAttributes w h g d di do' fgc _ b) = (PaperAttributes w h g d di do' fgc (Just bgc) b)
instance HasBorder PaperAttributes where
    border b (PaperAttributes w h g d di do' fgc bgc _) = (PaperAttributes w h g d di do' fgc bgc b)

-- Omit drawing of the background, use when all drawings will cover the entire
-- area:
paperNoBg (PaperAttributes w h g d di do' fgc _ b) = (PaperAttributes w h g d di do' fgc Nothing b)

data PaperState = PaperNorm [ScreenCmnd] | PaperClick Int Int [ScreenCmnd]

paper = paper' id

paper' :: Change PaperAttributes -> Gadget
paper' cba =
  let (PaperAttributes sx sy (gnox,gnoy) df pi po fgc bgc b) = 
	  cba (PaperAttributes 200 200 (10,10) transparent nci nco fgc' (Just bgc') 5) 
      sc = df (sx,sy) ((0,0),(sx,sy))
      (asx,asy) = (sx+b+b,sy+b+b)
      p state =
	rx [
	    fromSM $ \r -> case r of
	      SMMouseClick x y _ -> 
		if x>=b && x<=asx-b && y>=b && y<=asy-b then
		  case state of
		    PaperNorm sc ->
		      p (PaperClick (x-b) (y-b) sc)
		    PaperClick x1 y1 sc ->
		      let x' = x-b
			  y' = y-b
			  gl = gridlock ((x',y'),(x1,y1))
			  (del,sc') = nl (DrawLine gl) sc
			  nl (DrawLine l) (DrawLine h:t) =
	                      if h==l
                              then (True,t)
			      else (d,DrawLine h:t')
		            where (d,t') = nl (DrawLine l) t
			  nl l sc = (False,l:sc)
			  ud _ _ = moveOrigin (b,b) [colour "black",drawline gl]
	              in
		      if del then
			  newPic (boxed sc') $
			  tx po (scToDf (sx,sy) ((colour "black"):sc')) $
			  p (PaperNorm sc')
		      else
			  updPic ud (boxed sc') $
			  tx po (scToDf (sx,sy) ((colour "black"):sc')) $
			  p (PaperNorm sc')
		else
		    p state
	      SMMouseUnClick _ _ _ ->
		  p state,
	    from pi $ \df' ->
		let sc' = dfToSc (sx,sy) df' in
		newPic (boxed sc') $
		case state of
		    PaperNorm _ ->
			p (PaperNorm sc')
		    PaperClick x y _ ->
			p (PaperClick x y sc')
	] (rxFail "paper")
      newPic p c = txSM (SMDrawFun p) $ c
      updPic u p c = txSM (SMUpdateDrawFun u p) $ c
      (fgc',bgc',flc,shc,lic,hic,foc,enc,dic,c1,c2,c3,c4,c5) = colourset
      boxed pc (sx,sy) r = 
	  let x = sx - 1
	      y = sy - 1 in
	  plinth b shc lic enc (0,0) (x,y)++
	  (case bgc of
	     Nothing -> []
	     Just bgc ->
	       [DrawSetColour bgc,fillbox ((b,b),(x-b,y-b))]++
	       [DrawSetColour enc]++glx++gly++[DrawSetColour fgc])++
	  moveOrigin (b,b) pc
      gly = case gridy of
	      0 -> []
	      s -> [DrawLine ((b,y),(asx-b-1,y)) | y<-[b,b+s..asy-b]]
      glx = case gridx of
	      0 -> []
	      s -> [DrawLine ((x,b),(x,asy-b-1)) | x<-[b,b+s..asx-b]]
      gridx = sx `div` (gnox-1)
      gridy = sy `div` (gnoy-1)
      gridlock w@((x1,y1),(x2,y2)) = 
	  ((r gridx x1, r gridy y1),(r gridx x2, r gridy y2))
	      where
	      r s n = if s == 0 then n else n' - (n' `mod` s) 
		  where
		  n' = n+s `div` 2
  in
  initGadget (sx+b+b,sy+b+b) (boxed sc) $
  claim pi $
  p (PaperNorm sc)


iconedit :: Gadget
iconedit =
    wire $ \w ->
    wire $ \v ->
    spawn (typewriter (ip v)) $
    spawn (mapC (\f->f (10000,10000) ((0,0),(10000,10000))) (ip w) (op v)) $
    paper' (width 400.height 400.gridlock (10,10).pictureOut (op w))
