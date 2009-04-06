module Test where

--alltests = [wraptest,boxtest,buttontest,bargraphtest,papertest,gridtest,rowtest,childtest,reltest,newwintest,doactiontest,radiotest]

wraptest :: Gadget
wraptest =
    wrap (border 20) ((b1 <-> b2) <|> b3)
        where
        b1 = buttontest
        b2 = buttontest
        b3 = buttontest
 
boxtest :: Gadget
boxtest =
    box (width 200.height 200) ((b1 <-> b2) <|> b3)
        where
        b1 = buttontest
        b2 = buttontest
        b3 = buttontest
 
buttontest = button' (buttonPonPoff) nco ()

bargraphtest :: Gadget
bargraphtest =
    wire $ \w ->
    wire $ \v ->
    let b = button' (buttonMomentary) (op v) (\x->x+1)
	c = button' (buttonMomentary) (op w) (\x->x-1)
        g = bargraph id [ip w,ip v] in
    wrap (border 20) (b <-> g <-> c)

papertest :: Gadget
papertest =
    wire $ \a ->
    wire $ \b ->
    let p1 = paper (pictureIn (ip a).pictureOut (op b))
	p2 = paper (width 400.pictureIn (ip b).pictureOut (op a)) in
    wrap (border 20) (p1 <-> p2)

gridtest = grid ncs ncs ncs ncs (take 5 (repeat (take 5 (repeat b))))
    where
    b l t r b =
	wire $ \i ->
	wire $ \o ->
	spawn (merger (map fst [l,t,r,b]) (op i)) $
	spawn (broadcast (ip o) (map snd [l,t,r,b])) $
	paper (pictureIn (ip i).pictureOut (op o).width 50.height 50)
    nc = (nci,nco)
    ncs = take 5 (repeat nc)

rowtest = row nc ncs nc ncs (take 5 (repeat b))
    where
    b l t r b =
	wire $ \i ->
	wire $ \o ->
	spawn (merger (map fst [l,t,r,b]) (op i)) $
	spawn (broadcast (ip o) (map snd [l,t,r,b])) $
	paper (pictureIn (ip i).pictureOut (op o).width 50.height 50)
    nc = (nci,nco)
    ncs = take 5 (repeat nc)

{-
childtest :: Gadget
childtest =
    initGadget $
    spawnGadgetAt (10,10) (wrap 10 blank (buttontest <-> buttontest)) $
    let br (x1,y1) (x2,y2) = (x2-x1,y2-y1) in
    spawnGadgetRel br (wrap 10 blank buttontest) $
    let ini = 400 in
    setSize (ini,ini) $
    txSM (SMDrawFun blank) $
    ct ini where
    ct s =
	rx [
	    fromSM $ \m -> case m of
		SMMouseClick _ _ 2 ->
		    setSize (s+30,s+30) $
		    ct (s+30)
		SMMouseClick _ _ 3 ->
		    setSize (s-30,s-30) $
		    ct (s-30)
		otherwise ->
		    ct s
	] (rxFail "childtest")

reltest :: Gadget
reltest =
    initGadget $
    let br (x1,y1) (x2,y2) = (x2-x1,y2-y1) in
    spawnGadgetRel br (wrap 10 blank (buttontest <-> buttontest)) $
    r where
    r :: Gadget
    r =
	txSM (SMDrawFun blank) $
	let ini = 100 in
	setSize (ini,ini) $
	ct ini where
	ct s =
	    rx [
		fromSM $ \m -> case m of
		    SMMouseClick _ _ 2 ->
			setSize (s+30,s+30) $
			ct (s+30)
		    SMMouseClick _ _ 3 ->
			setSize (s-30,s-30) $
			ct (s-30)
		    otherwise ->
			ct s
	    ] (rxFail "reltest")

newwintest :: Gadget
newwintest =
    initGadget $
    let br (x1,y1) (x2,y2) = (x2-x1-10,y2-y1-10) in
    duplex $ \((smrs,smrq),wsm) ->
    claim smrs $
    newWindow wsm br $
    tx smrq (SMDrawFun bonwbox) $
    --tx smrq SMVisible $
    txSM (SMDrawFun blank) $
    let ini = 400 in
    let inis = 50 in
    tx smrq (SMMySize (inis,inis)) $
    setSize (ini,ini) $
    n ini inis smrs smrq where
    n s ss smrs smrq = 
	rx [
	    from smrs $ \r -> case r of
		SMMouseClick _ _ 1 ->
		    txSM (SMDrawFun blackbg) $
		    n s ss smrs smrq 
		SMMouseUnClick _ _ 1 ->
		    txSM (SMDrawFun blank) $
		    n s ss smrs smrq
		SMMouseClick _ _ 2 ->
		    tx smrq (SMMySize (ss+30,ss+30)) $
		    n s (ss+30) smrs smrq
		SMMouseClick _ _ 3 ->
		    tx smrq (SMMySize (ss-30,ss-30)) $
		    n s (ss-30) smrs smrq
		otherwise ->
		    n s ss smrs smrq,
	    fromSM $ \r -> case r of
		SMMouseClick _ _ 2 ->
		    setSize (s+30,s+30) $
		    n (s+30) ss smrs smrq
		SMMouseClick _ _ 3 ->
		    setSize (s-30,s-30) $
		    n (s-30) ss smrs smrq
		otherwise ->
		    n s ss smrs smrq
	] (rxFail "newwintest")
-}

doactiontest :: Gadget
doactiontest =
    getGadgetWires $ \(_,_,(smi,_)) ->
    let smo = opFromIp smi in
    txSM (SMDoAction (tx smo (SMMouseClick 0 0 1))) $
    buttontest

radiotest :: Gadget
radiotest =
    radioButtons 5 $ \b ->
    let b' = map (\b->b id nco ()) b in
    wrap (border 20) (beside b')

t = [(lap,"lap1"),(lap,"lap2")]

lap =
    wrap (border 20) (a <-> b)
	where 
	a = button' (buttonPonPoff) nco ()
	b = button' (xcol (col "red")) nco ()

slidertest =
    let h = sliderx id
	v = slidery id
    in
    (setGapSize 0 $ wrap id (h <.> v),"slider test")

tv g =
    wire $ \p ->
    wire $ \p' ->
    wire $ \s ->
    wire $ \s' ->
    let v = viewer g (100,100) nci (ip p) (op s)
	m = button (op p) (20,20)
	z = editor' (editorInput (ip s').editorInit "")
    in
    spawn (mapC (const.show) (ip s) (op s')) $
    wrap (border 20) (m <-> v <-> z)

testwall g = (testwall' g, "Wall Test")
testwall' g =
    wire $ \a ->
    let fn = "*lucida-medium-r-*-120-*"
	co = col "black"
	b = button' (picture (text fn co "create")) (op a) (uncurry window g)
	w = wall (ip a)
    in
    wrap (border 20) (b <-> w)
