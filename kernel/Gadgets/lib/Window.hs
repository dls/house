module Window where
import Prelude hiding (Maybe)
import GadgetsPrelude
import Components
import Os
import Display

window :: Gadget -> String -> Window
window g title =
    myNameIs "window" $
    readState $ \(WindowState _ (_,(lors,lorq),_) osc gap) ->
    claim lors $
    duplex $ \(gw,wg) ->
    claim (fst wg) $
    spawnWithState g (GadgetState (nco,gw,(nci,nco)) osc gap) $
    duplex $ \(wsm,smw) ->
    let me = opFromIp (fst smw)
    in
    rx [
        from (fst wg) $ \l -> case l of
            LOInit s fm d cs ->
                let s' = pairop (+) s (4,24)
		    fm' p = [moveImage me p]
                    ca (x,y) b c = tx me (SMMouseClick x y b) $ c
                    uca (x,y) b c = tx me (SMMouseUnClick x y b) $ c
                    d' p = [mkImage me (p,pairop (+) s' p) dw True ca uca (d (2,22)) False]
		    dw = drawWindow enc
                    cs' = wsm:cs
                in
                claim (fst smw) $
		setGadgetWires (snd smw,(lors,lorq),smw) $
                tx lorq (LOInit s' fm' d' cs') $
                wc None lorq me wg
    ] (rxFail "wrap")
    where
    wc :: Maybe Coord -> Out LORequest -> ImageID -> InOut LORequest LOResponse -> Window
    wc op lorq me (rq,rs) =
	let wc' np = wc np lorq me (rq,rs) in
        rx [
	    fromSM $ \r -> case r of
		SMMouseClick x y _ ->
		    txSM (SMDrawFun (drawWindow hic)) $
		    wc' (Yes (x,y))
		SMMouseUnClick x y _ ->
		    case op of
			None ->
			    wc' None
			Yes (ox,oy) ->
			    alterPos (x-ox,y-oy) $
			    txSM (SMDrawFun (drawWindow enc)) $
			    wc' None,
            from rq $ \l -> case l of
                LOSize s fm fo ->
                    let s' = pairop (+) s (4,24)
                        fm' p = (moveImage me p):fm (2,22)
                        fo' p = (moveImage me p):(resizeImage me s'):fo (2,22)
                    in
                    tx lorq (LOSize s' fm' fo') $
                    wc' op
                LOInit _ _ _ _ ->
                    error "wrap: wrapped gadget sent another LOInit"
        ] (rxFail "wrap")
    (fgc,bgc,flc,shc,lic,hic,foc,enc,dic,c1,c2,c3,c4,c5) = colourset
    drawWindow i (x',y') _ = 
	let x = x'-1
	    y = y'-1 in
	plinth 2 lic shc i (0,20) (x,y-20)++
	plinth 4 lic shc i (0,0) (x,19)++
	[DrawSetColour flc,fillbox ((4,4),(x-4,15)),fillbox ((2,22),(x-4,y-24))]++
	[DrawSetColour fgc]++ctext fn (x,16) title
    fn = "*-lucida-bold-r-*-*-*-120-*-iso8859-1"

go size d g = launch d (os size (map (uncurry window) g))

posGo size d posgs =
    launch d (posOs size [(Just p,window g t)|(p,(g,t))<-posgs])
