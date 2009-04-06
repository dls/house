module Explode where
import Prelude hiding (Maybe(..),sequence)
import Data.List((\\))
import Gadgets

data Player = Black | White deriving (Eq,Show)

players = [Black, White]

opponent Black = White
opponent White = Black

type Invade = InOut Player Player

explode_game :: Int -> Int -> Gadget
explode_game x y =
    myNameIs "explode" $
    let n = x * y in
    wire $ \who ->
    duplexes n $ \p ->
    let (s,c) = unzip p
	cs = zipWith (zipWith cell) (caps x y) (gen c)
	caps xmax ymax = [[(length.filter id) [x>1,y>1,x<xmax,y<ymax]|x<-[1..xmax]]|y<-[1..ymax]]
	gen l = f:gen r
	    where
	    (f,r) = splitAt x l
	ncx = replicate x (nci,nco)
	ncy = replicate y (nci,nco) in
    spawn (referee s (op who)) $
    setGapSize 20 $
    wrap' (border 20) (grid ncy ncx ncy ncx cs 
			    <|> 
		    showColour (ip who))

data Turn = Turn Player | NoTurn | Win Player

showColour :: In Turn -> Gadget
showColour who = 
    wire $ \d ->
    let trans d = case d of
		    NoTurn -> const ""
		    Turn p -> const (show p++"'s turn")
		    Win p -> const (show p++" wins!") in
    spawn (mapC trans who (op d)) $
    editor' (editorInit "setting up".disabled.border 1.width 150.height 30.editorInput (ip d))

explode x y = (explode_game x y,"Explode!")

data Notify = ClickOver (Maybe Player) | Bonk (Maybe Player) | Boom Int (Maybe Player)
data Ruling = Invasion Player | ClearSmoke

{-
cell :: Int -> InOut Ruling Notify -> Invade -> Invade -> Invade -> Invade -> Gadget
cell n tr (li,lo) (ti,tO) (ri,ro) (bi,bo) =
    meterIn li $ \li' lim ->
    meterIn ti $ \ti' tim ->
    meterIn ri $ \ri' rim ->
    meterIn bi $ \bi' bim ->
    meterOut lo $ \lo' lom ->
    meterOut tO $ \to' tom ->
    meterOut ro $ \ro' rom ->
    meterOut bo $ \bo' bom ->
    let c = cell2 n tr (li',lo') (ti',to') (ri',ro') (bi',bo') in
    wrap (setGapSize 0 $ (tim <-> tom) <|> ((lim <|> lom) <-> c <-> (rim <|> rom)) <|> (bim <-> bom))
-}

cell :: Int -> InOut Ruling Notify -> Invade -> Invade -> Invade -> Invade -> Gadget
cell capacity (fromReferee,toReferee) l t r b =
    wire $ \d ->
    wire $ \c ->
    giveImage (button' (border 5.height 54.width 54.picture (stones Black 0).
    --giveImage (combination "12" (border 5.height 54.width 54.picture (stones Black 0).
			buttonMomentary.pictureIn (ip d)) (op c) ()) $
    myNameIs "cell" $
    let draw = op d
	click = ip c
	cell' o = 
	    let neighbours = [l,t,r,b]
		invasion p o =
		    let (op,n') = case o of
			    None -> (None,1)
			    Yes (p,n) -> (Yes p,n+1) in
		    tx draw (stones p n') $
		    if n' < capacity then
			tx toReferee (Bonk op) $
			cell' (Yes (p,n'))
		    else
			tx draw boom $
			tx toReferee (Boom capacity op) $
			sequence (map (flip tx p.snd) neighbours) $
			cell' None in
	    rx [
		from click $ \_ ->
		    let o' = case o of
			       None -> None
			       Yes (p,_) -> Yes p in
		    tx toReferee (ClickOver o') $
		    cell' o,
		from fromReferee $ \m -> case m of
		    Invasion p -> 
			invasion p o
		    ClearSmoke -> 
			tx draw (stones Black 0) $
			cell' o,
		froms neighbours $ \p _ ->
		    invasion p o
	    ] (rxFail "cell")
    in
    claim fromReferee $
    claim click $
    sequence (map (claim.fst) [l,t,r,b]) $
    cell' None

type CellID = Out Ruling

referee :: [InOut Notify Ruling] -> Out Turn -> Component
referee cs turn =
    myNameIs "referee" $
    sequence (map (claim.fst) cs) $
    tx turn (Turn Black) $
    p Black 0 everyone [] ih where
    p :: Player -> Int -> [CellID] -> [CellID] -> [(Player,Int)] -> Component
    -- current tally unexploded_this_go smoke_filled no_of_cells_containing_each_colour
    p c t b s h =
	rx [
	    froms cs $ \r toCell -> 
		let b' = b \\ [toCell] 
		    s' = s \\ [toCell] 
		    t' = t - 1 
		    newcount h o n = map (adj (+1) n.adj (\x->x-1) o) h
		    adj _ None h = h
		    adj f (Yes np) (p,n) = if p==np then (p,f n) else (p,n)
		in case r of
		    ClickOver v ->
			let same = case v of
				None -> True
				Yes cp -> cp == c
			in if same && t == 0 then
			    tx turn NoTurn $
			    tx toCell (Invasion c) $
			    p c 1 b s h
			else
			    p c t b s h
		    Bonk pc ->
			let h' = newcount h pc (Yes c) in
			if t' == 0 then
			    if (any (==nc) (map snd h')) then
				tx turn (Win c) $
				end
			    else
				let c' = opponent c in
				tx turn (Turn c') $
				sequence (map (flip tx ClearSmoke) s') $
				p c' 0 everyone [] h'
			else
			    p c t' b s' h'
		    Boom n pc ->
			if b' == [] then
			    tx turn (Win c) $
			    end
			else
			    p c (t'+n) b' (toCell:s) (newcount h pc None)
	] (rxFail "playControl")
    everyone = map snd cs
    ih = zip players (repeat 0)
    nc = length cs

stones :: Player -> Int -> DrawFun
stones p n (xs,ys) _ = empt ++ atms
    where
    empt = [] --[colour "white", fillbox ((0,0),(xs-1,ys-1))]
    atms = case n of
	    0 -> []
	    1 -> colour "black":concat [a ((x 3,y 3),(x 6,y 6))]
	    2 -> colour "black":concat [a ((x 1,y 3),(x 4,y 6)),a ((x 5,y 3),(x 8,y 6))]
	    3 -> colour "black":concat [a ((x 3,y 1),(x 6,y 4)),a ((x 1,y 5),(x 4,y 8)),
		    a ((x 5,y 5),(x 8,y 8))]
	    4 -> colour "black":concat [a ((x 1,y 1),(x 4,y 4)),a ((x 5,y 1),(x 8,y 4)),
		    a ((x 1,y 5),(x 4,y 8)),a ((x 5,y 5),(x 8,y 8))]
    x q = q * (xs `div` 9)
    y q = q * (ys `div` 9)
    a ((x1,y1),(x2,y2)) = 
	let (x3,y3) = (x2-1,y2-1) in
	case p of
	    Black -> [colour "black",fillbox ((x1,y1),(x3,y3))]
	    White -> [colour "black",l ((x1,y1),(x3,y1)),l ((x3,y1),(x3,y3)), 
		    l ((x3,y3),(x1,y3)), l ((x1,y3),(x1,y1)), colour "white",
		    fillbox ((x1+1,y1+1),(x3-1,y3-1))]
    l = drawline

boom :: DrawFun
boom (xs,ys) _ =
    let xs' = xs - 1
	ys' = ys - 1 
	ax = 3 * (xs `div` 8)
	ay = 3 * (ys `div` 8)
	bx = xs `div` 8
	by = ys `div` 8 in
    [   colour "black",
	drawline ((bx,by),(ax,ay)),
	drawline ((xs'-bx,by),(xs'-ax,ay)),
	drawline ((bx,ys'-by),(ax,ys'-ay)),
	drawline ((xs'-bx,ys'-by),(xs'-ax,ys'-ay))
    ]
