module Components where
import GadgetsPrelude
import Display

class ComponentClass s where
    rx :: [Guarded s] -> Process s -> Process s
    rx ps = primRx (concat ps)
    from :: In m -> (m -> Process s) -> Guarded s
    from p c = [primFrom (const p) c]
    froms :: [(In m,b)] -> (m -> b -> Process s) -> Guarded s
    froms p c = [primFrom (const i) $ \m -> c m d | (i,d) <- p]
    tx :: Out m -> m -> Process s -> Process s
    tx = primTx
    claim :: In m -> Process s -> Process s
    --claim (In p) c = typeout ("claim "++show p++"\n") $ primClaim (In p) c
    claim = primClaim
    disown :: In m -> Process s -> Process s
    --disown (In p) c = typeout ("disown "++show p++"\n") $ primDisown (In p) c
    disown = primDisown
    wire :: (Wire m -> Process s) -> Process s
    wire = primWire
    wires :: Int -> ([Wire m] -> Process s) -> Process s
    wires = primWires
    launch :: GadgetDisplay -> Process s -> IO ()
    launch d p = primLaunch d p (error "tried to evaluate the state of the launch component")
    end :: Process s
    end = primTerminate
    duplex :: (Duplex x y -> Process s) -> Process s
    duplex c =
	wire $ \x ->
	wire $ \y ->
	c ((ip x,op y),(ip y,op x))
    duplexes :: Int -> ([Duplex x y] -> Process s) -> Process s
    duplexes n c =
	accumulate (replicate n duplex) $
	c
    wiresIn :: [In a -> c] -> ([c] -> [Out a] -> Process s) -> Process s
    wiresIn cs c = wi [] [] cs
	where 
	wi cc we [] = c (reverse cc) (reverse we)
	wi cc we (h:t) = 
	    wire $ \w -> 
	    wi ((h (ip w)):cc) ((op w):we) t
    wiresOut :: [Out a -> c] -> ([c] -> [In a] -> Process s) -> Process s
    wiresOut cs c = wo [] [] cs
	where 
	wo cc we [] = c (reverse cc) (reverse we)
	wo cc we (h:t) = 
	    wire $ \w -> 
	    wo ((h (op w)):cc) ((ip w):we) t
    wiresInOut :: [InOut a b -> c] -> ([c] -> [InOut b a] -> Process s) -> Process s
    wiresInOut cs c = wo [] [] cs
	where 
	wo cc we [] = c (reverse cc) (reverse we)
	wo cc we (h:t) = 
	    duplex $ \(v,w) -> 
	    wo ((h v):cc) (w:we) t
    initState :: Process s -> Process s
    initState = setState (error "tried to evaluate process state before it has been set")
    spawn :: Process a -> Process s -> Process s
    spawn p = primSpawn p (error "tried to evaluate process state before setting")
    spawnWithState :: Process a -> a -> Process s -> Process s
    spawnWithState = primSpawn
    spawnComponent :: Process ComponentState -> Process s -> Process s
    spawnComponent c = spawn (initState c)
    getGapSize :: (Int -> Process s) -> Process s
    setGapSize :: Int -> Process s -> Process s
    loop :: Procedure s -> Process s
    loop l = l $ loop l
    loopState :: a -> (a -> Function a s) -> Process s
    loopState s f = f s $ (\s' -> loopState s' f)
    when :: Bool -> (Process s -> Process s) -> Process s -> Process s
    when p a c = if p then a c else c

instance ComponentClass ComponentState where
    initState = setState (ComponentState initGap)
    getGapSize c = readState $ \ (ComponentState g) -> c g
    setGapSize g = setState (ComponentState g)

class ComponentClass s => SystemClass s where
    getSystemWires :: (SystemWires -> Process s) -> Process s
    setSystemWires :: SystemWires -> Process s -> Process s
    txOS :: OSRequest -> (Process s) -> Process s
    txOS rq c = 
	getSystemWires $ \(_,osrq) ->
	tx osrq rq $
	c
    spawnSystem :: SystemClass t => Process t -> Process s -> Process s
    spawnSystem s c = 
	duplex $ \(osg,gos) ->
	txOS (OSNewCon osg) $
	let s' =
		initState $
		setSystemWires gos $
		s in
	spawn s' $
	c

instance ComponentClass SystemState where
    initState = setState (SystemState (nci,nco) initGap)
    getGapSize c = readState $ \ (SystemState _ g) -> c g
    setGapSize g = updateState $ \ (SystemState x _) -> SystemState x g

instance SystemClass SystemState where
    getSystemWires c = readState $ \ (SystemState w _) -> c w
    setSystemWires w = updateState $ \ (SystemState _ g) -> SystemState w g

--getGadgetWires :: (GadgetWires -> Process s) -> Process s
getGadgetWires c = readState (c . gadgetWires)

class SystemClass s => GadgetClass s where
    gadgetWires :: s -> GadgetWires
    setGadgetWires :: GadgetWires -> Process s -> Process s
    txSM :: SMRequest -> Process s -> Process s
    txSM rq c =
	getGadgetWires $ \(_,_,(_,smrq)) ->
	tx smrq rq $
	c
    fromSM :: (SMResponse -> Process s) -> Guarded s
    fromSM c = 
	let f = primFrom ((\(_,_,(smrs,_)) ->smrs).gadgetWires) c in
	[f]
    txLO :: LORequest -> Process s -> Process s
    txLO rq c =
	getGadgetWires $ \(_,(_,lorq),_) ->
	tx lorq rq $
	c
    setSize :: Size -> Process s -> Process s
    setSize s c =
	getGadgetWires $ \(_,(_,ms),(smrs,smrq)) ->
	let imid = opFromIp smrs 
	    fm p = [moveImage imid p]
	    fo p = [moveImage imid p, resizeImage imid s]
	in
	tx ms (LOSize s fm fo) $
	c
    spawnGadget :: GadgetClass t => InOut LOResponse LORequest -> Process t -> Process s -> Process s
    spawnGadget l g c =
	getGadgetWires $ \(p,_,smc) ->
	getSystemWires $ \osc ->
	let g' =
		initState $
		setGadgetWires (p,l,smc) $
		setSystemWires osc $
		g in
	spawn g' $
	c
    {-
    spawnGadgetOf :: GadgetClass t => Out SMRequest -> InOut LOResponse LORequest -> 
			Process t -> Process s -> Process s
    spawnGadgetOf p l g c =
	getGadgetWires $ \(p,_,smc) ->
	getSystemWires $ \osc ->
	let g' =
		initState $
		setGadgetWires (p,l,smc) $
		setSystemWires osc $
		g in
	spawn g' $
	c
    -}
    {-
    spawnGadgetAt :: GadgetClass t => Coord -> Process t -> Process s -> Process s
    spawnGadgetAt (x,y) = spawnGadgetRel (\_ _->(x,y))
    spawnGadgetRel :: GadgetClass t => (Size->Size->Coord) -> Process t -> Process s -> Process s
    spawnGadgetRel f g c =
	duplex $ \(osg,gos) ->
	duplex $ \(smg,gsm) ->
	txOS (OSNewCon osg) $
	txSM (SMNewCon smg) $
	getGadgetWires $ \(pa,la@(mt,ms),sm) ->
	wire $ \ogl ->
	duplex $ \(ngl,nlg) ->
	setGadgetWires (pa,(mt,op ogl),sm) $
	let pc :: Out LORequest -> In LORequest -> InOut LORequest LOResponse -> Component
	    pc ms pms (cms,cmt) = 
		claim pms $
		claim cms $
		p None None where
		p mcs mps =
		    --typeout (show mcs++" "++show mps++"\n") $
		    rx [
			from pms $ \l -> case l of
			    LOSize ps ->
				tx ms (LOSize ps) $
				( case mcs of
				    None -> id
				    Yes cs -> tx cmt (LOMoveTo (f cs ps)) ) $
				p mcs (Yes ps),
			from cms $ \l -> case l of
			    LOSize cs ->
				( case mps of
				    None -> id
				    Yes ps -> tx cmt (LOMoveTo (f cs ps)) ) $
				p (Yes cs) mps
		    ] (rxFail "spawnGadgetAt layout controller") 
	in
	spawn (pc ms (ip ogl) nlg) $
	let g' =
		initState $
		setGadgetWires (snd gsm,ngl,gsm) $
		setSystemWires gos $
		g in
	spawn g' $
	c
    -}
    giveImage :: Process s -> System -> Process s
    giveImage g c =
	spawnSystem c $
	g
    {-
    newWindow :: InOut SMRequest SMResponse -> (Size->Size->Coord) -> Process s -> Process s
    newWindow smrr f c =
	getGadgetWires $ \(pa,la@(mt,ms),sm) ->
	wire $ \pms ->
	duplex $ \(csm,smc) ->
	txSM (SMNewCon smc) $

	setGadgetWires (pa,(mt,op pms),sm) $
	let pc :: Out LORequest -> In LORequest -> InOut SMRequest SMResponse -> InOut SMResponse SMRequest -> Component
	    pc ms pms (rqi,rso) (rsi,rqo) = 
		claim pms $
		claim rqi $
		claim rsi $
		p None None where
		p mcs mps =
		    --typeout (show mcs++" "++show mps++"\n") $
		    rx [
			from pms $ \l -> case l of
			    LOSize ps ->
				tx ms (LOSize ps) $
				( case mcs of
				    None -> id
				    Yes cs -> tx rqo (SMMoveTo (f cs ps)) ) $
				p mcs (Yes ps),
			from rqi $ \r -> case r of
			    SMMySize cs ->
				tx rqo r $
				( case mps of
				    None -> id
				    Yes ps -> tx rqo (SMMoveTo (f cs ps)) ) $
				p (Yes cs) mps
			    o -> 
				tx rqo o $
				p mcs mps,
			from rsi $ \r ->
			    tx rso r $
			    p mcs mps
		    ] (rxFail "newWindow layout controller") in
	spawn (pc ms (ip pms) smrr csm) $
	c
    -}

instance ComponentClass GadgetState where
    initState = setState (GadgetState (nco,(nci,nco),(nci,nco)) (nci,nco) initGap)
    getGapSize c = readState (c . ( \ (GadgetState _ _ g) -> g))
    setGapSize g = updateState $ \ (GadgetState x y _) -> GadgetState x y g
    {-
    rx p f =
	getGadgetWires $ \(_,(mt,_),(_,smrq)) ->
	let mv = 
		from mt $ \l -> case l of
		    LOMoveTo c -> 
			tx smrq (SMMoveTo c) $ 
			rx p f 
	in
	primRx (concat (mv:p)) f
    -}
instance SystemClass GadgetState where
    getSystemWires c = readState (c . ( \ (GadgetState _ w g) -> w))
    setSystemWires sw = updateState $ \ (GadgetState gw _ g) -> GadgetState gw sw g

instance GadgetClass GadgetState where
    gadgetWires (GadgetState w _ _) = w
    setGadgetWires gw = updateState $ \ (GadgetState _ sw g) -> GadgetState gw sw g

class GadgetClass s => WindowClass s where
    getWindowWires :: (WindowWires -> Process s) -> Process s
    setWindowWires :: WindowWires -> Process s -> Process s
    alterPos :: Coord -> Process s -> Process s
    alterPos p c =
	getWindowWires $ \mp ->
	tx mp p $
	c
    fromLO :: (LOResponse -> Process s) -> Guarded s
    fromLO c =
	let f = primFrom ((\ (_,(mt,_),_) ->mt).gadgetWires) c in
	[f]
    spawnWindow :: WindowClass t => Out Coord -> InOut LOResponse LORequest -> Process t -> Process s -> Process s
    spawnWindow p l m c =
	duplex $ \(osg,gos) ->
	duplex $ \(smg,gsm) ->
	txOS (OSNewCon osg) $
	txSM (SMNewCon smg) $
	let m' =
		setWindowWires p $
		setGadgetWires (snd gsm,l,gsm) $
		setSystemWires gos $
		m in
	spawn m' $
	c

instance ComponentClass WindowState where
    initState = setState (WindowState nco (nco,(nci,nco),(nci,nco)) (nci,nco) initGap)
    getGapSize c = readState ( c . ( \ (WindowState _ _ _ g) -> g))
    setGapSize g = updateState $ \ (WindowState x y z _) -> WindowState x y z g
    rx p f =
	getGadgetWires $ \(_,(mt,_),(_,smrq)) ->
	let mv = 
		from mt $ \l -> case l of
		    LOMoveTo c -> 
			tx smrq (SMMoveTo c) $ 
			rx p f 
	in
	primRx (concat (mv:p)) f
instance SystemClass WindowState where
    getSystemWires c = readState ( c . ( \ (WindowState _ _ w g) -> w))
    setSystemWires sw = updateState $ \ (WindowState mw gw _ g) -> WindowState mw gw sw g
instance GadgetClass WindowState where
    gadgetWires (WindowState _ gw _ _) = gw
--    getGadgetWires c = readState (c . ( \ (WindowState _ w _ g) -> w))
    setGadgetWires gw = updateState $ \ (WindowState mw _ sw g) -> WindowState mw gw sw g
instance WindowClass WindowState where
    getWindowWires c = readState (c . ( \ (WindowState w _ _ g) -> w))
    setWindowWires mw = updateState $ \ (WindowState _ gw sw g) -> WindowState mw gw sw g

-- Initialising functions:

initSystem c =
    getSystemWires $ \(osi,_) ->
    claim osi $
    c

initGadget :: Size -> DrawFun -> Gadget -> Gadget
initGadget s df c =
    getGadgetWires $ \(_,(mt,ms),_) ->
    claim mt $
    duplex $ \(smg,gsm) ->
    claim (fst gsm) $
    setGadgetWires (snd gsm,(mt,ms),gsm) $

    let imid = opFromIp (fst gsm)
	ca (x,y) b c = tx imid (SMMouseClick x y b) $ c
	uca (x,y) b c = tx imid (SMMouseUnClick x y b) $ c
	d p@(px,py) = [mkImage imid (p,pairop (+) p s) df True ca uca [] False]
	fm p = [moveImage imid p]
    in
    tx ms (LOInit s fm d [smg]) $

    getSystemWires $ \(posrs,posrq) ->
    duplex $ \(osg,gos) ->
    tx posrq (OSNewCon osg) $
    setSystemWires gos $
    claim (fst gos) $
    c

initParent :: Size -> DrawFun -> [Display] -> [InOut SMRequest SMResponse] -> Bool -> Gadget -> Gadget
initParent s df ch co ov c =
    getGadgetWires $ \(_,(mt,ms),_) ->
    claim mt $
    duplex $ \(smg,gsm) ->
    claim (fst gsm) $
    setGadgetWires (snd gsm,(mt,ms),gsm) $

    let imid = opFromIp (fst gsm)
	ca (x,y) b c = tx imid (SMMouseClick x y b) $ c
	uca (x,y) b c = tx imid (SMMouseUnClick x y b) $ c
	d p = [mkImage imid (p,pairop (+) p s) df True ca uca ch ov]
	fm p = [moveImage imid p]
    in
    tx ms (LOInit s fm d (smg:co)) $

    getSystemWires $ \(posrs,posrq) ->
    duplex $ \(osg,gos) ->
    tx posrq (OSNewCon osg) $
    setSystemWires gos $
    claim (fst gos) $
    c

initWindow c =
    getSystemWires $ \(osi,_) ->
    getGadgetWires $ \(_,(mt,_),(smi,_)) ->
    claim mt $
    claim smi $
    claim osi $
    c

-- Gadget attribute defaults stuff

-- attributes set when Gadget started up

class HasWidth a where
    width :: Int -> Change a
class HasHeight a where
    height :: Int -> Change a
class HasBorder a where
    border :: Int -> Change a
class HasPicture a where
    picture :: DrawFun -> Change a
class HasSize a where
    size :: Size -> Change a
class HasNumber a where
    number :: Int -> Change a
class HasGridLock a where
    gridlock :: (Int,Int) -> Change a
class HasFGColour a where
    fgcol :: Colour -> Change a
class HasBGColour a where
    bgcol :: Colour -> Change a
class HasXColour a where
    xcol :: Colour -> Change a
class HasYColour a where
    ycol :: Colour -> Change a
class HasZColour a where
    zcol :: Colour -> Change a
class HasOperational a where
    operational :: Bool -> Change a
class HasFont a where
    fontwmetrics :: String -> (String -> (Int,(Int,Int))) -> Change a

font fn = fontwmetrics fn (textSize fn) -- textSize only works "fixed" :-(

-- attributes that can be reset during operation by sending a msg.
class HasPictureIn a where
    pictureIn :: In DrawFun -> Change a
class HasOnOff a where
    onoff :: In Bool -> Change a

-- attributes that Gadget can change and notify by sending out a msg.
class HasPictureOut a where
    pictureOut :: Out DrawFun -> Change a

enabled :: HasOperational a => Change a
enabled = operational True

disabled :: HasOperational a => Change a
disabled = operational False

