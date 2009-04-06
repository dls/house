module Container where
import GadgetsPrelude
import Components
import Button
import Display
import Area(subtractArea)
import Useful(mapC)
import Layout

data WrapAttributes = WrapAttributes Int DrawFun

instance HasBorder WrapAttributes where
    border b (WrapAttributes _ df) = (WrapAttributes b df)
instance HasPicture WrapAttributes where
    picture df (WrapAttributes b _) = (WrapAttributes b df)

wrap = wrap' id

wrap' :: Change WrapAttributes -> Gadget -> Gadget
wrap' cwa g =
    myNameIs "wrap" $
    readState $ \(GadgetState (_,(lors,lorq),_) osc gap) ->
    -- create g with layout wires connected to me
    duplex $ \(gw,wg) ->
    claim (fst wg) $
    spawnWithState g (GadgetState (nco,gw,(nci,nco)) osc gap) $
    duplex $ \(wsm,smw) ->
    let (fgc,bgc,flc,shc,lic,hic,foc,enc,dic,c1,c2,c3,c4,c5) = colourset
	(WrapAttributes b df) = cwa (WrapAttributes 0 (colourbox flc))
	me = opFromIp (fst smw)
	wc :: Out LORequest -> ImageID -> In LORequest -> In SMResponse -> Gadget
	wc lorq me rq smrq =
	    let wc' = wc lorq me rq smrq in
	    rx [
		from rq $ \l -> case l of
		    LOSize s fm fo ->
			let s' = pairop (+) s (db,db)
			    fm' p = (moveImage me p):fm (b,b)
			    fo' p = (moveImage me p):(resizeImage me s'):fo (b,b)
			in
			tx lorq (LOSize s' fm' fo') $
			wc'
		    LOInit _ _ _ _ ->
			error "wrap: wrapped gadget sent another LOInit",
		from smrq $ \_ ->
		    wc'
	    ] (rxFail "wrap")
	db = b+b
    in
    rx [
	from (fst wg) $ \l -> case l of
	    LOInit s fm d cs ->
		let s' = pairop (+) (db,db) s
		    fm' p = [moveImage me p]
		    ca _ _ c = c
		    uca _ _ c = c
		    d' p = [mkImage me (p,pairop (+) s' p) df True ca uca (d (b,b)) False]
		    cs' = wsm:cs
		in
		claim (fst smw) $
		setGadgetWires (snd smw,(lors,lorq),smw) $
		tx lorq (LOInit s' fm' d' cs') $
		wc lorq me (fst wg) (fst smw)
    ] (rxFail "wrap")
    
data BoxAttributes = BoxAttributes Size DrawFun

instance HasSize BoxAttributes where
    size s (BoxAttributes _ df) = (BoxAttributes s df)
instance HasPicture BoxAttributes where
    picture df (BoxAttributes s _) = (BoxAttributes s df)
instance HasWidth BoxAttributes where
    width w (BoxAttributes (_,h) df) = (BoxAttributes (w,h) df)
instance HasHeight BoxAttributes where
    height h (BoxAttributes (w,_) df) = (BoxAttributes (w,h) df)

box = box' id

box' :: Change BoxAttributes -> Gadget -> Gadget
box' cwa g =
    myNameIs "box" $
    readState $ \(GadgetState (_,(lors,lorq),_) osc gap) ->
    -- create g with layout wires connected to me
    duplex $ \(gw,wg) ->
    claim (fst wg) $
    spawnWithState g (GadgetState (nco,gw,(nci,nco)) osc gap) $
    duplex $ \(wsm,smw) ->
    let (fgc,bgc,flc,shc,lic,hic,foc,enc,dic,c1,c2,c3,c4,c5) = colourset
	(BoxAttributes (bx,by) df) = cwa (BoxAttributes (0,0) (colourbox flc))
	me = opFromIp (fst smw)
	wc :: Out LORequest -> ImageID -> In LORequest -> In SMResponse -> Gadget
	wc lorq me rq smrq =
	    let wc' = wc lorq me rq smrq in
	    rx [
		from rq $ \l -> case l of
		    LOSize (cx,cy) fm fo ->
			let (ax,px) = if cx > bx then (cx,0) else (bx,(bx-cx)`div`2)
			    (ay,py) = if cy > by then (cy,0) else (by,(by-cy)`div`2)
			    fm' p = (moveImage me p):fm (px,py)
			    fo' p = (moveImage me p):(resizeImage me (ax,ay)):fo (px,py)
			in
			tx lorq (LOSize (ax,ay) fm' fo') $
			wc'
		    LOInit _ _ _ _ ->
			error "box: boxed gadget sent another LOInit",
		from smrq $ \_ ->
		    wc'
	    ] (rxFail "box")
    in
    rx [
	from (fst wg) $ \l -> case l of
	    LOInit (cx,cy) fm d cs ->
		let (ax,px) = if cx > bx then (cx,0) else (bx,(bx-cx)`div`2)
		    (ay,py) = if cy > by then (cy,0) else (by,(by-cy)`div`2)
		    fm' p = [moveImage me p]
		    ca _ _ c = c
		    uca _ _ c = c
		    d' p = [mkImage me (p,pairop (+) (ax,ay) p) df True ca uca (d (px,py)) False]
		    cs' = wsm:cs
		in
		claim (fst smw) $
		setGadgetWires (snd smw,(lors,lorq),smw) $
		tx lorq (LOInit (ax,ay) fm' d' cs') $
		wc lorq me (fst wg) (fst smw)
    ] (rxFail "box")
    
-- SLIDERS --

-- width height wholeX wholeY visibleX visibleY posX posY 
-- wholeXIn wholeYIn visibleXIn visibleYIn posXIn posYIn 
data Slider = Slider Int Int Int Int Int Int Int Int 
		    (In Int) (In Int) (In (Change Int)) 
			(In (Change Int)) (In (Change Int)) (In (Change Int))
			    (Out Int) (Out Int) (In (Change Size))

instance HasWidth Slider where
    width w (Slider _ h wx wy vx vy px py wxi wyi vxi vyi pxi pyi pxo pyo dsi) =
	(Slider w h wx wy vx vy px py wxi wyi vxi vyi pxi pyi pxo pyo dsi)
instance HasHeight Slider where
    height h (Slider w _ wx wy vx vy px py wxi wyi vxi vyi pxi pyi pxo pyo dsi) =
	(Slider w h wx wy vx vy px py wxi wyi vxi vyi pxi pyi pxo pyo dsi)

sliderWholeX :: Int -> Change Slider
sliderWholeX wx (Slider w h _ wy vx vy px py wxi wyi vxi vyi pxi pyi pxo pyo dsi) =
    (Slider w h wx wy vx vy px py wxi wyi vxi vyi pxi pyi pxo pyo dsi)

sliderWholeY :: Int -> Change Slider
sliderWholeY wy (Slider w h wx _ vx vy px py wxi wyi vxi vyi pxi pyi pxo pyo dsi) =
    (Slider w h wx wy vx vy px py wxi wyi vxi vyi pxi pyi pxo pyo dsi)

sliderVisibleX :: Int -> Change Slider
sliderVisibleX vx (Slider w h wx wy _ vy px py wxi wyi vxi vyi pxi pyi pxo pyo dsi) =
    (Slider w h wx wy vx vy px py wxi wyi vxi vyi pxi pyi pxo pyo dsi)

sliderVisibleY :: Int -> Change Slider
sliderVisibleY vy (Slider w h wx wy vx _ px py wxi wyi vxi vyi pxi pyi pxo pyo dsi) =
    (Slider w h wx wy vx vy px py wxi wyi vxi vyi pxi pyi pxo pyo dsi)

sliderPosX :: Int -> Change Slider
sliderPosX px (Slider w h wx wy vx vy _ py wxi wyi vxi vyi pxi pyi pxo pyo dsi) =
    (Slider w h wx wy vx vy px py wxi wyi vxi vyi pxi pyi pxo pyo dsi)

sliderPosY :: Int -> Change Slider
sliderPosY py (Slider w h wx wy vx vy px _ wxi wyi vxi vyi pxi pyi pxo pyo dsi) =
    (Slider w h wx wy vx vy px py wxi wyi vxi vyi pxi pyi pxo pyo dsi)

sliderWholeXIn :: In Int -> Change Slider
sliderWholeXIn wxi (Slider w h wx wy vx vy px py _ wyi vxi vyi pxi pyi pxo pyo dsi) =
    (Slider w h wx wy vx vy px py wxi wyi vxi vyi pxi pyi pxo pyo dsi)

sliderWholeYIn :: In Int -> Change Slider
sliderWholeYIn wyi (Slider w h wx wy vx vy px py wxi _ vxi vyi pxi pyi pxo pyo dsi) =
    (Slider w h wx wy vx vy px py wxi wyi vxi vyi pxi pyi pxo pyo dsi)

sliderVisibleXIn :: In (Change Int) -> Change Slider
sliderVisibleXIn vxi (Slider w h wx wy vx vy px py wxi wyi _ vyi pxi pyi pxo pyo dsi) =
    (Slider w h wx wy vx vy px py wxi wyi vxi vyi pxi pyi pxo pyo dsi)

sliderVisibleYIn :: In (Change Int) -> Change Slider
sliderVisibleYIn vyi (Slider w h wx wy vx vy px py wxi wyi vxi _ pxi pyi pxo pyo dsi) =
    (Slider w h wx wy vx vy px py wxi wyi vxi vyi pxi pyi pxo pyo dsi)

sliderPosXIn :: In (Change Int) -> Change Slider
sliderPosXIn pxi (Slider w h wx wy vx vy px py wxi wyi vxi vyi _ pyi pxo pyo dsi) =
    (Slider w h wx wy vx vy px py wxi wyi vxi vyi pxi pyi pxo pyo dsi)

sliderPosYIn :: In (Change Int) -> Change Slider
sliderPosYIn pyi (Slider w h wx wy vx vy px py wxi wyi vxi vyi pxi _ pxo pyo dsi) =
    (Slider w h wx wy vx vy px py wxi wyi vxi vyi pxi pyi pxo pyo dsi)

sliderPosXOut :: Out Int -> Change Slider
sliderPosXOut pxo (Slider w h wx wy vx vy px py wxi wyi vxi vyi pxi pyi _ pyo dsi) =
    (Slider w h wx wy vx vy px py wxi wyi vxi vyi pxi pyi pxo pyo dsi)

sliderPosYOut :: Out Int -> Change Slider
sliderPosYOut pyo (Slider w h wx wy vx vy px py wxi wyi vxi vyi pxi pyi pxo _ dsi) =
    (Slider w h wx wy vx vy px py wxi wyi vxi vyi pxi pyi pxo pyo dsi)

sliderChangeSize :: In (Change Size) -> Change Slider
sliderChangeSize dsi (Slider w h wx wy vx vy px py wxi wyi vxi vyi pxi pyi pxo pyo _) =
    (Slider w h wx wy vx vy px py wxi wyi vxi vyi pxi pyi pxo pyo dsi)

slider = slider' id

slider' :: Change Slider -> Gadget
slider' cs =
    let (Slider w h wx wy vx vy px py wxi wyi vxi vyi pxi pyi pxo pyo ds) =
	    cs (Slider 100 100 1 1 1 1 0 0 nci nci nci nci nci nci nco nco nci)
    in
    claim wxi $
    claim wyi $
    claim vxi $
    claim vyi $
    claim pxi $
    claim pyi $
    claim ds $
    let s sx sy wx wy vx vy px py mp =
	    rx [
		from ds $ \dsf ->
		    let (sx',sy') = dsf (sx,sy) in
		    setSize (sx',sy') $
		    rds sx' sy' wx wy vx vy px py mp,
		from wxi $ \wx' ->
		    rds sx sy wx' wy vx vy px py mp,
		from wyi $ \wy' ->
		    rds sx sy wx wy' vx vy px py mp,
		from vxi $ \dvx ->
		    rds sx sy wx wy (dvx vx) vy px py mp,
		from vyi $ \dvy ->
		    rds sx sy wx wy vx (dvy vy) px py mp,
		from pxi $ \dpx ->
		    rds sx sy wx wy vx vy (dpx px) py mp,
		from pyi $ \dpy ->
		    rds sx sy wx wy vx vy px (dpy py) mp,
		fromSM $ \r -> case r of
		    SMMouseClick x1 y1 b -> 
			txSM (SMDrawFun (slidedf True w h wx wy vx vy px py)) $
			s sx sy wx wy vx vy px py (Yes (x1,y1))
		    SMMouseUnClick x2 y2 b -> 
			case mp of
			    Yes (x1,y1) ->
				let (dx,dy) = (((x2-x1)*wx) `div` sx, ((y2-y1)*wy) `div` sy)
				    (px2,py2) = (px+dx,py+dy)
				    (wx',wy',vx',vy',px',py') = confine wx wy vx vy px2 py2 in
				txSM (SMDrawFun (slidedf False w h wx' wy' vx' vy' px' py')) $
				when (px' /= px) (tx pxo px') $
				when (py' /= py) (tx pyo py') $
				s sx sy wx' wy' vx' vy' px' py' None
			    None -> 
				txSM (SMDrawFun (slidedf False w h wx wy vx vy px py)) $
				s sx sy wx wy vx vy px py mp
		    otherwise ->
			s sx sy wx wy vx vy px py mp
	    ] (rxFail "slider")
	rds sx' sy' wx wy vx vy px py mp =
	    let (wx',wy',vx',vy',px',py') = confine wx wy vx vy px py in
	    txSM (SMDrawFun (slidedf False w h wx' wy' vx' vy' px' py')) $
	    tx pxo px' $
	    tx pyo py' $
	    s sx' sy' wx' wy' vx' vy' px' py' mp
	(fgc,bgc,flc,shc,lic,hic,foc,enc,dic,c1,c2,c3,c4,c5) = colourset
	slidedf :: Bool -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> DrawFun
	slidedf p _ _ wx wy vx vy px py (w,h) _ =
	    let w' = w - 6
		h' = h - 6
		x = 3 + ((px*w') `div` wx)
		y = 3 + ((py*h') `div` wy)
		sx = ((vx*w') `div` wx) - 1
		sy = ((vy*h') `div` wy) - 1
		(ca,cb,cc) = if p then (shc,lic,hic) else (shc,lic,enc)
	    in
	    plinth 3 shc lic flc (0,0) (w-1,h-1)++
	    plinth 3 ca cb cc (x,y) (sx,sy)++
	    [DrawSetColour bgc,fillbox ((x+3,y+3),(x+sx-3,y+sy-3)),DrawSetColour flc]++
	    concat (map fillrect (subtractArea ((x,y),(x+sx+1,y+sy+1)) [((3,3),(w-3,h-3))]))
	confine :: Int -> Int -> Int -> Int -> Int -> Int -> (Int,Int,Int,Int,Int,Int)
	confine wx wy vx vy px py =
	    let px' = if px+vx > wx then wx-vx else if px < 0 then 0 else px
		py' = if py+vy > wy then wy-vy else if py < 0 then 0 else py in
	    (wx,wy,vx,vy,px',py')
    in
    initGadget (w,h) (slidedf False w h wx wy vx vy px py) $
    s w h wx wy vx vy px py None

sliderx = sliderx' id

sliderx' :: Change Slider -> Gadget
sliderx' cs = 
    wire $ \px ->
    wire $ \sc ->
    let ds = (Slider 1 1 1 1 1 1 0 0 nci nci nci nci nci nci nco nco nci)
	(Slider w h _ _ _ _ _ _ _ _ _ _ _ _ _ _ si) = cs ds
	h' = h - 6
	bl = button' (width h'.height h'.border 3.picture leftarrow.buttonMomentary) (op px) ((+)(-10))
	br = button' (width h'.height h'.border 3.picture rightarrow.buttonMomentary) (op px) ((+)10)
	s = slider' (sliderChangeSize (ip sc).sliderPosXIn (ip px).width (w-h-h).height h.cs)
    in
    setGapSize 0 $
    spawn (mapC (\f -> (\(x,y)->(fst (f (x,y)),y))) si (op sc)) $
    wrap (bl <-> s <-> br)

slidery = slidery' id

slidery' :: Change Slider -> Gadget
slidery' cs = 
    wire $ \py ->
    wire $ \sc ->
    let ds = (Slider 1 1 1 1 1 1 0 0 nci nci nci nci nci nci nco nco nci)
	(Slider w h _ _ _ _ _ _ _ _ _ _ _ _ _ _ si) = cs ds
	w' = w - 6
	bu = button' (width w'.height w'.border 3.picture uparrow.buttonMomentary) (op py) ((+)(-10))
	bd = button' (width w'.height w'.border 3.picture downarrow.buttonMomentary) (op py) ((+)10)
	s = slider' (sliderChangeSize (ip sc).sliderPosYIn (ip py).width w.height (h-w-w).cs)
    in
    setGapSize 0 $
    spawn (mapC (\f -> (\(x,y)->(x,snd (f (x,y))))) si (op sc)) $
    wrap (bu <|> s <|> bd)

uparrow (x,y) _ = 
    let hx = x `div` 2
	tqx = (7*x) `div` 8
	qx = tqx `div` 7
	tqy = (6*y) `div` 8
	qy = tqy `div` 6
	(fgc',bgc',flc,shc,lic,hic,foc,enc,dic,c1,c2,c3,c4,c5) = colourset
    in
    [ DrawSetColour shc,
	DrawFilledTriangle ((hx,qy),(tqx,tqy),(qx,tqy))]

rightarrow = turnCW uparrow
downarrow = turnCW rightarrow
leftarrow = turnCW downarrow

viewer :: Gadget -> Size -> In (Change Size) -> In Coord -> Out Size -> Gadget
viewer g (sx,sy) ns i o =
    myNameIs "viewer" $
    readState $ \(GadgetState (_,(lors,lorq),_) osc gap) ->
    -- create g with layout wires connected to me
    duplex $ \(gv,vg) ->
    claim (fst vg) $
    spawnWithState g (GadgetState (nco,gv,(nci,nco)) osc gap) $
    duplex $ \(wsm,smw) ->
    let me = opFromIp (fst smw)
	vc :: Size -> Coord -> (Coord -> [DisplayChange]) -> In Coord -> Out Size -> In LORequest -> In SMResponse -> Gadget
	vc (sx,sy) (px,py) fm i o rq smrq =
	    let vc' s' p' = vc s' p' fm i o rq smrq in
	    rx [
		from rq $ \l -> case l of
		    LOSize (cx,cy) fm fo ->
			tx o (cx,cy) $
			let fm' p = [moveImage me p]
			    fo' p = fo (px,py)
			in
			tx lorq (LOSize (sx,sy) fm' fo') $
			vc' (sx,sy) (px,py)
		    LOInit _ _ _ _ ->
			error "box: boxed gadget sent another LOInit",
		from smrq $ \_ ->
		    vc' (sx,sy) (px,py),
		from i $ \p -> 
		    tx (snd smw) (SMUpdates (fm p)) $
		    vc' (sx,sy) p,
		from ns $ \ds -> 
		    let s' = ds (sx,sy)
			fm' p = [moveImage me p]
			fo' p = [resizeImage me s']
		    in
		    tx lorq (LOSize s' fm' fo') $
		    vc' s' (px,py)
	    ] (rxFail "viewer")
    in
    rx [
	from (fst vg) $ \l -> case l of
	    LOInit (cx,cy) fm d cs ->
		let fm' p = [moveImage me p]
		    ca _ _ c = c
		    uca _ _ c = c
		    d' p = [mkImage me (p,pairop (+) (sx,sy) p) blank True ca uca (d (0,0)) False]
		    cs' = wsm:cs
		in
		claim (fst smw) $
		setGadgetWires (snd smw,(lors,lorq),smw) $
		tx lorq (LOInit (sx,sy) fm' d' cs') $
		tx o (cx,cy) $
		claim i $
		claim ns $
		vc (sx,sy) (0,0) fm i o (fst vg) (fst smw)
    ] (rxFail "viewer")

fixedViewController :: In Size -> Out Coord -> In Int -> Out Int -> In Int -> Out Int -> Component
fixedViewController gs gp vp vs hp hs =
    myNameIs "fVC" $
    claim gs $
    claim vp $
    claim hp $
    fvc (0,0) where
    fvc (ph,pv) = 
	rx [
	    from gs $ \(sh,sv) ->
		tx hs sh $
		tx vs sv $
		fvc (ph,pv) ,
	    from hp $ \ph' ->
		tx gp (-ph',-pv) $
		fvc (ph',pv),
	    from vp $ \pv' ->
		tx gp (-ph,-pv') $
		fvc (ph,pv')
	] (rxFail "fixedViewController")

variableViewController :: In Size -> Out Coord -> In Int -> Out Int -> Out (Change Int) -> Out (Change Size) -> In Int -> Out Int -> Out (Change Int) -> Out (Change Size) -> In (Change Size) -> Out (Change Size) -> Component
variableViewController gs gp vp vs vvs vss hp hs hvs hss ds cs =
    myNameIs "fVC" $
    claim gs $
    claim vp $
    claim hp $
    claim ds $
    fvc (0,0) where
    fvc (ph,pv) = 
	rx [
	    from gs $ \(sh,sv) ->
		tx hs sh $
		tx vs sv $
		fvc (ph,pv) ,
	    from hp $ \ph' ->
		tx gp (-ph',-pv) $
		fvc (ph',pv),
	    from vp $ \pv' ->
		tx gp (-ph,-pv') $
		fvc (ph,pv'),
	    from ds $ \dsf ->
		tx cs dsf $
		tx hss dsf $
		tx vss dsf $
		tx hvs (\s -> (fst (dsf (s,0)))) $
		tx vvs (\s -> (snd (dsf (0,s)))) $
		fvc (ph,pv)
	] (rxFail "fixedViewController")

fixedScrollBox :: Size -> Gadget -> Gadget
fixedScrollBox (sx,sy) g =
    wire $ \gs ->
    wire $ \gp ->
    wire $ \xs ->
    wire $ \xp ->
    wire $ \ys ->
    wire $ \yp ->
    let x = sliderx' (sliderVisibleX sx.sliderPosX 0.sliderWholeXIn (ip xs).sliderPosXOut (op xp).
			sliderWholeY 1.sliderVisibleY 1.
			width sx.height 29)
	y = slidery' (sliderVisibleY sy.sliderPosY 0.sliderWholeYIn (ip ys).sliderPosYOut (op yp).
			sliderWholeX 1.sliderVisibleX 1.
			height (sy+29).width 29)
	v = viewer g (sx,sy) nci (ip gp) (op gs)
    in
    spawn (fixedViewController (ip gs) (op gp) (ip yp) (op ys) (ip xp) (op xs)) $
    setGapSize 0 $
    wrap' (border 0) ((v <|> x) <-> y)

dragIcon = dragIcon' id

dragIcon' :: Change ButtonAttributes -> Out (Change Coord) -> Gadget
dragIcon' cba o =
    wire $ \w ->
    let ca False (SMMouseClick x y _) c = tx (op w) (x,y) $ c True
	ca True (SMMouseUnClick x y _) c = tx (op w) (x,y) $ c False
	ca s _ c = c s
    in
    let di =
	    rx [
		from (ip w) $ \(x1,y1) ->
		    rx [
			from (ip w) $ \(x2,y2) ->
			    tx o (\(x,y) -> (x+x2-x1,y+y2-y1)) $
			    di
		    ] (rxFail "dragIcon")
	    ] (rxFail "dragIcon") 
	df (x,y) _ =
	    let tl = (x `div` 10,y `div` 10)
		ls = ((8*x) `div` 10,(8*y) `div` 10)
		ss = ((5*x) `div` 10,(5*y) `div` 10)
		(fgc,bgc,flc,shc,lic,hic,foc,enc,dic,c1,c2,c3,c4,c5) = colourset
	    in
	    [DrawSetColour flc,fillbox ((0,0),(x-1,y-1))]++
	    plinth 2 lic shc enc tl ss++
	    plinth 2 lic shc enc tl ls
    in
    giveImage (button' (picture df.buttonAction ca.cba) nco ()) $
    myNameIs "dragIcon" $
    claim (ip w) $
    di
	
variableScrollBox :: Size -> Gadget -> Gadget
variableScrollBox (sx,sy) g =
    wire $ \gs ->
    wire $ \gp ->
    wire $ \xs ->
    wire $ \vxs ->
    wire $ \xp ->
    wire $ \ys ->
    wire $ \vys ->
    wire $ \yp ->
    wire $ \d ->
    wire $ \vs ->
    wire $ \cxs ->
    wire $ \cys ->
    let x = sliderx' (sliderVisibleX sx.sliderPosX 0.sliderWholeXIn (ip xs).sliderPosXOut (op xp).
			sliderWholeY 1.sliderVisibleY 1.sliderVisibleXIn (ip vxs).
			sliderChangeSize (ip cxs).width sx.height 29)
	y = slidery' (sliderVisibleY sy.sliderPosY 0.sliderWholeYIn (ip ys).sliderPosYOut (op yp).
			sliderWholeX 1.sliderVisibleX 1.sliderVisibleYIn (ip vys).
			sliderChangeSize (ip cys).height sy.width 29)
	v = viewer g (sx,sy) (ip vs) (ip gp) (op gs)
	b = dragIcon' (picture blank.width 23.height 23.border 3) (op d)
    in
    spawn (variableViewController (ip gs) (op gp) (ip yp) (op ys) (op vys) (op cys) (ip xp) (op xs) (op vxs) (op cxs) (ip d) (op vs)) $
    setGapSize 0 $
    wrap' (border 0) ((v <-> y) <|> (x <-> b))
	
