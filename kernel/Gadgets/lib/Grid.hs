module Grid where
import GadgetsPrelude
import Layout
import Components(duplex,duplexes)

grid :: [InOut m m] -> [InOut m m] -> [InOut m m] -> [InOut m m] -> [[InOut m m -> InOut m m -> InOut m m -> InOut m m -> Gadget]] -> Gadget
grid l t r b g =
    myNameIs "grid" $
    --wrap id (grid' l t r b g)
    (grid' l t r b g)
grid' [l] t [r] b [g] = row' l t r b g
grid' l t r b gs =
    let z _ _ _ _ [] gs = above (reverse gs)
	z (l:ls) t (r:rs) b (g:gs) a =
	    let n = length g in
	    duplexes n $ \i ->
	    let (it,ib) = unzip i
		cr = row' l t r it g in
	    z ls ib rs b gs (cr:a)
	z _ _ _ _ g _ = error ("grid': mismatch between #connections and #gadgets") in
    z l t r b gs []

row :: InOut m m -> [InOut m m] -> InOut m m -> [InOut m m] -> [InOut m m -> InOut m m -> InOut m m -> InOut m m -> Gadget] -> Gadget
row l t r b g = 
    myNameIs "row" $
    --wrap id (row' l t r b g)
    (row' l t r b g)
row' l [t] r [b] [g] = g l t r b
row' l t r b gs = 
    let z _ _ _ _ [] gs = beside (reverse gs)
	z l (t:ts) r (b:bs) (g:gs) a =
	    duplex $ \(il,ir) ->
	    let cg = g l t il b in
	    z ir ts r bs gs (cg:a)
	z _ _ _ _ _ _ = error "row': mismatch between # of connections and # of gadgets" in
    z l t r b gs []
