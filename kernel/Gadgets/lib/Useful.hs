module Useful where
import Prelude hiding (sequence)
import GadgetsPrelude
import Components

-- ****** Useful components

collect :: [In m] -> Out [m] -> Component
collect is o =
    myNameIs "collect" $
    sequence (map claim is) $
    c (take l (repeat [])) where
    c bs =
	if all (not.null) bs then
	    let os = map head bs
	        bs' = map tail bs in
	    tx o os $
	    c bs'
	else
	    rx [
		from i $ \v -> 
	    	    let store _ [] _ = error "bug in collect"
			store 1 (q:qs) v = (q++[v]):qs
			store n (q:qs) v = q:store (n-1) qs v
	        	bs' = store n bs v in
	    	    c bs' | (i,n) <- zip is [1..l]
	    ] (rxFail "collect")
    l = length is

newset :: [In m] -> Out [m] -> Component
newset is o =
    myNameIs "newset" $
    sequence (map claim is) $
    i (take l (repeat None)) where
    i bs =
	if all isYes bs then
	    t bs
	else
	    r bs $
	    i
    t bs = 
	let os = map (\(Yes n)->n) bs in
	tx o os $
	r bs $
	t
    l = length is
    r bs c =
	rx [
	    froms tis $ \v n -> 
		let bs' = h++(Yes v):t
			where (h,(_:t)) = splitAt n bs
		in
		c bs'
	] (rxFail "newset")
    tis = zip is [0..]

merger :: [In m] -> Out m -> Component
merger is o =
    myNameIs "merger" $
    sequence (map claim is) $
    m where
    m =
        rx [
	    from i $ \v ->
		tx o v $
		m | i <- is
	    ] (rxFail "merger")

eet i j o = merger [i,j] o

broadcast :: In m -> [Out m] -> Component
broadcast i os =
    myNameIs "broadcast" $
    claim i $
    b where
    b =
	rx [
	    from i $ \v ->
		sequence [tx o v | o <- os] $
		b
	    ] (rxFail "broadcast")

tee i o p = broadcast i [o,p]

changeToConst :: a -> In (Change a) -> Out a -> Component
changeToConst a i o =
    claim i $
    ctc a
    where
    ctc a =
	rx [
	    from i $ \f ->
		let a' = f a in
		tx o a' $
		ctc a'
	] (rxFail "changeToConst")

constToChange :: In a -> Out (Change a) -> Component
constToChange = mapC const

{-
mapC :: (i->j) -> In i -> Out j -> Component
mapC f i o =
    claim i $
    m where
    m =
	rx [
	    from i $ \v ->
		tx o (f v) $
		m
	    ] (rxFail "mapC")
-}

mapC :: (i->j) -> In i -> Out j -> Component
mapC f i o =
    claim i $
    loop (\m ->
	rx [
	    from i $ \v ->
		tx o (f v) $
		m
	    ] (rxFail "mapC")
    )

-- *** NB: a generator component tends to mess up component
-- *** scheduling somewhat.

generator :: [m] -> Out m -> Component
generator l o =
    sequence [tx o m | m<-l] $
    end

typewriter :: Show m => In m -> Component
typewriter i =
    claim i $
    d where
    d =
        rx [
            from i $ \s ->
                typeout (show s++"\n") $
                d
            ] (rxFail "display")

trap :: In t -> In m -> Out m -> Component
trap t i o =
    claim t $
    claim i $
    trap' None where
    trap' h =
	rx [
	    from i $ \m ->
		trap' (Yes m),
	    from t $ \_ ->
		case h of
		    None -> 
			trap' None
		    Yes m ->
			tx o m $
			trap' None
	] (rxFail "trap")

memory :: In t -> In m -> Out m -> Component
memory t i o =
    claim t $
    claim i $
    memory' None where
    memory' h =
	rx [
	    from i $ \m ->
		typeout "memory\n" $
		memory' (Yes m),
	    from t $ \_ ->
		typeout "trigger\n" $
		case h of
		    None -> 
			memory' None
		    Yes m ->
			tx o m $
			memory' h
	] (rxFail "memory")

buffer :: [m] -> In m -> Out m -> Component
buffer ms i o =
    claim i $
    sequence (map (tx o) ms) $
    buffer' where
    buffer' = 
	rx [
	    from i $ \m ->
		tx o m $
		buffer'
	] (rxFail "buffer")

gate :: In Bool -> In m -> Out m -> Component
gate r i o =
    claim r $
    claim i $
    gate' False where
    gate' g =
	rx [
	    from i $ \m ->
		if g then
		    tx o m $
		    gate' g
		else
		    gate' g,
	    from r $ \g' ->
		gate' g'
	] (rxFail "gate")

apply :: In (m->m) -> In m -> Out m -> Component
apply m i o =
    claim m $
    claim i $
    apply' None None where
    apply' (Yes mo) (Yes va) =
	tx o (mo va) $
	apply' None None
    apply' mo va =
	rx [
	    from i $ \va' ->
		apply' mo (Yes va'),
	    from m $ \mo' ->
		apply' (Yes mo') va
	] (rxFail "gate")

