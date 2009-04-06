module Layout where
import Prelude hiding (sequence)
import Data.List(sort,unzip4)
import GadgetsPrelude
import Components

infixr 2 <|>, <.>, <^>
infixr 3 <->, <<>, <>>

(<->)  = layoutBi horLO
(<.>)  = layoutBi botLO
(<^>)  = layoutBi topLO
(<|>)  = layoutBi vertLO
(<<>)  = layoutBi leftLO
(<>>)  = layoutBi rightLO
beside = layoutWith horLO
bottom = layoutWith botLO
top    = layoutWith topLO
above  = layoutWith vertLO
left   = layoutWith leftLO
right  = layoutWith rightLO

matrix :: [[Gadget]] -> Gadget
matrix l = above (map beside l)

layoutWith :: (Int->[InOut LORequest LOResponse]->InOut LOResponse LORequest->Component) -> [Gadget] -> Gadget
layoutWith _ [] = error "no gadgets beside each other!"
layoutWith lof gs =
    myNameIs "layoutWith" $
    getGadgetWires $ \(p,(mt,ms),smc) ->
    getSystemWires $ \osc ->
    getGapSize $ \gap ->
    let z g c = 
	    duplex $ \(lg,gl) ->
	    spawnWithState g (GadgetState (p,lg,smc) osc gap) $
	    c gl in
    accumulate (map z gs) $ \gls ->
    spawn (lof gap gls (mt,ms)) $
    end

layoutBi :: (Int->[InOut LORequest LOResponse]->InOut LOResponse LORequest->Component) -> Gadget -> Gadget -> Gadget
layoutBi lof l r =
    myNameIs "layoutBi" $
    getGadgetWires $ \(p,(mt,ms),smc) ->
    getSystemWires $ \osc ->
    getGapSize $ \gap ->
    duplex $ \(lg,gl) ->
    duplex $ \(lg2,gl2) ->
    spawnWithState l (GadgetState (p,lg,smc) osc gap) $
    spawnWithState r (GadgetState (p,lg2,smc) osc gap) $
    spawn (lof gap [gl,gl2] (mt,ms)) $
    end

-- The functions that define the operations behind <->, <|>, beside, above, etc.

type PlaceFun = Int -> Int -> (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
type SizeFun = Int -> Int -> [(Int,Int)] -> (Int,Int)

layout :: PlaceFun -> SizeFun -> Int -> [InOut LORequest LOResponse] -> InOut LOResponse LORequest -> Component
layout place size g gll (mt,ms) =
    myNameIs "layout controller" $
    claim mt $
    sequence (map claim gms) $
    im (replicate len None) where
    im a =
	if all isYes a then
	    let f (Yes (LOInit s fm d c)) = 
		    (d,s,fm,c)
		f _ = error "layout: a gadget didn't send LOInit first"
		(dss,ss,fms,cs) = unzip4 (map f a)
		pos p = place g len p ss
		ts = size g len ss
		fm p = concat (zipWith (\f g->f g) fms (pos p))
		ds p = concat (zipWith (\f g->f g) dss (pos p))
	    in
	    tx ms (LOInit ts fm ds (concat cs)) $
	    lo (zip ss fms)
	else
	    rx [
		froms i $ \lorq gn ->
		    let a' = h++((Yes lorq):t)
			(h,_:t) = splitAt gn a
		    in
		    im a'
	    ] (rxFail "layout controller (im)")
    lo a =
	rx [
	    froms i $ \l gn -> case l of
		LOSize s fm fo ->
		    let a' = h++((s,fm):t)
			(h,_:t) = splitAt gn a
			(ss,fms) = unzip a'
			pos p = place g len p ss
			ts = size g len ss
			fm' p = concat (zipWith (\f g->f g) fms (pos p))
			(fmh,_:fmt) = splitAt gn fms
			fo' p = concat (zipWith (\f g->f g) (fmh++fo:fmt) (pos p))
		    in
		    tx ms (LOSize ts fm' fo') $
		    lo a'
		LOInit _ _ _ _ ->
		    error "layout controller received another LOInit"
	] (rxFail "layout")
    i = zip gms [0..]
    (gms,gmt) = unzip gll
    len = length gll

--dynlayout :: PlaceFun -> SizeFun -> Int -> In (InOut LORequest LOResponse) -> [InOut LORequest LOResponse] -> InOut LOResponse LORequest -> Component

horLO = layout phor shor
    where
    phor g n (x,y) ss =
        let my = last (sort (map snd ss))
	    adj _ _ [] = []
            adj g (x,y) ((sx,sy):r) = (x,y+((my-sy)`div`2)):adj g (x+g+sx,y) r in
        adj g (x,y) ss
    shor g n ss =
        let my = last (sort (map snd ss))
	    tg = (n-1)*g
    	    tsx = tg+(sum (map fst ss)) in
        (tsx,my)

topLO = layout phor shor
    where
    phor g n (x,y) ss =
        let my = last (sort (map snd ss))
	    adj _ _ [] = []
            adj g (x,y) ((sx,sy):r) = (x,y):adj g (x+g+sx,y) r in
        adj g (x,y) ss
    shor g n ss =
        let my = last (sort (map snd ss))
	    tg = (n-1)*g
    	    tsx = tg+(sum (map fst ss)) in
        (tsx,my)

botLO = layout phor shor
    where
    phor g n (x,y) ss =
        let my = last (sort (map snd ss))
	    adj _ _ [] = []
            adj g (x,y) ((sx,sy):r) = (x,y+my-sy):adj g (x+g+sx,y) r in
        adj g (x,y) ss
    shor g n ss =
        let my = last (sort (map snd ss))
	    tg = (n-1)*g
    	    tsx = tg+(sum (map fst ss)) in
        (tsx,my)

vertLO = layout pver sver
    where
    pver g n (x,y) ss =
        let mx = last (sort (map fst ss))
	    adj _ _ [] = []
            adj g (x,y) ((sx,sy):r) = (x+((mx-sx)`div`2),y):adj g (x,y+g+sy) r in
        adj g (x,y) ss
    sver g n ss =
        let mx = last (sort (map fst ss))
	    tg = (n-1)*g
    	    tsy = tg+(sum (map snd ss)) in
        (mx,tsy)

leftLO = layout pver sver
    where
    pver g n (x,y) ss =
        let mx = last (sort (map fst ss))
	    adj _ _ [] = []
            adj g (x,y) ((sx,sy):r) = (x,y):adj g (x,y+g+sy) r in
        adj g (x,y) ss
    sver g n ss =
        let mx = last (sort (map fst ss))
	    tg = (n-1)*g
    	    tsy = tg+(sum (map snd ss)) in
        (mx,tsy)

rightLO = layout pver sver
    where
    pver g n (x,y) ss =
        let mx = last (sort (map fst ss))
	    adj _ _ [] = []
            adj g (x,y) ((sx,sy):r) = (x+mx-sx,y):adj g (x,y+g+sy) r in
        adj g (x,y) ss
    sver g n ss =
        let mx = last (sort (map fst ss))
	    tg = (n-1)*g
    	    tsy = tg+(sum (map snd ss)) in
        (mx,tsy)

