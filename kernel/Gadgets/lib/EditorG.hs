module EditorG where
import Prelude hiding (Maybe(..))
import Data.Char(ord,chr,isDigit)
import GadgetsPrelude
import Components
default(Int)

{-
class Text d => Displayable d where
    toString :: d -> String
    toString = show

instance Displayable String where
    toString s = s
-}

class (Eq e,Show e) => Enterable e where
    prokey :: String -> Char -> (String, Bool, Maybe e)
    toString :: e -> String
    toString = show

instance Enterable String where
    prokey s k =
	if k == chr 8 || k == chr 127 then
	    let s' = init s in
	    if null s then (s,False,None) else (s',True,Yes s')
	else if k >= chr 32 && k <= chr 126 then
	    let s' = s ++ [k] in
	    (s',True,Yes s')
	else
	    (s,False,Yes s)
    toString s = s

instance Enterable Int where
    prokey s k =
	if k == chr 8 || k == chr 127 then
	    let s' = init s in
	    if null s then (s,False,None) else (s',True,Yes (stringToInt s'))
	else if isDigit k || (k=='-' && s=="") then
	    let s' = s ++ [k] in
	    (s',True,Yes (stringToInt s'))
	else
	    (s,False,Yes (stringToInt s))

stringToInt :: String -> Int
stringToInt s@(h:t) = if h=='-' then -(sti 0 t) else sti 0 s
    where
    sti n "" = n
    sti n (c:cs) =
	if isDigit c then
	    sti ((n*10)+(ord c - ord '0')) cs
	else
	    error "non-digit in stringToInt"

instance Enterable Float where
    prokey s k =
	if k == chr 8 || k == chr 127 then
	    let s' = init s in
	    if null s then (s,False,None) else (s',True,Yes (stringToFloat s'))
	else if isDigit k || (k == '.' && (not.contains '.') s) then 
	    let s' = s ++ [k] in
	    (s',True,Yes (stringToFloat s'))
	else
	    (s,False,Yes (stringToFloat s))

contains :: Eq a => a -> [a] -> Bool
contains c [] = False
contains c (a:bs) = if c==a then True else contains c bs

stringToFloat :: String -> Float
stringToFloat s = stf 0.0 s
    where
    stf n "" = n
    stf n (c:cs) =
	if isDigit c then
	    stf ((n*10.0)+(fromIntegral (ord c - ord '0'))) cs
	else if c == '.' then
	    let std n _ "" = n 
		std n d (c:cs) =
		    if isDigit c then
			std (n+((fromIntegral (ord c - ord '0'))/d)) (d*10.0) cs
		    else
			error "non-digit in stringToFloat"
	    in
	    std n 10.0 cs
	else
	    error "not digit or '.' in stringToFloat"

data Enterable e => EditorAttributes e = EditorAttributes Int 
    Int Int Colour Colour Colour e (In (Change e)) (Out (Change e)) 
    Bool (In Bool) (String->Char->Maybe (Gadget->Gadget)) Bool

instance Enterable e => HasWidth (EditorAttributes e) where
    width w (EditorAttributes _ h b fgc bgc xc e ei eo o oi s ct) =
        (EditorAttributes w h b fgc bgc xc e ei eo o oi s ct)
instance Enterable e => HasHeight (EditorAttributes e) where
    height h (EditorAttributes w _ b fgc bgc xc e ei eo o oi s ct) =
        (EditorAttributes w h b fgc bgc xc e ei eo o oi s ct)
instance Enterable e => HasBorder (EditorAttributes e) where
    border b (EditorAttributes w h _ fgc bgc xc e ei eo o oi s ct) =
        (EditorAttributes w h b fgc bgc xc e ei eo o oi s ct)
instance Enterable e => HasFGColour (EditorAttributes e) where
    fgcol fgc (EditorAttributes w h b _ bgc xc e ei eo o oi s ct) =
        (EditorAttributes w h b fgc bgc xc e ei eo o oi s ct)
instance Enterable e => HasBGColour (EditorAttributes e) where
    bgcol bgc (EditorAttributes w h b fgc _ xc e ei eo o oi s ct) =
        (EditorAttributes w h b fgc bgc xc e ei eo o oi s ct)
instance Enterable e => HasXColour (EditorAttributes e) where
    xcol xc (EditorAttributes w h b fgc bgc _ e ei eo o oi s ct) =
        (EditorAttributes w h b fgc bgc xc e ei eo o oi s ct)
instance Enterable e => HasOnOff (EditorAttributes e) where
    onoff oi (EditorAttributes w h b fgc bgc xc e ei eo o _ s ct) =
        (EditorAttributes w h b fgc bgc xc e ei eo o oi s ct)
instance Enterable e => HasOperational (EditorAttributes e) where
    operational o (EditorAttributes w h b fgc bgc xc e ei eo _ oi s ct) =
	(EditorAttributes w h b fgc bgc xc e ei eo o oi s ct)

editorSend :: Enterable e => (String->Char->Maybe (Gadget->Gadget)) -> Change (EditorAttributes e)
editorSend s (EditorAttributes w h b fgc bgc xc e ei eo o oi _ ct) =
    (EditorAttributes w h b fgc bgc xc e ei eo o oi s ct)

editorSendOnCR :: Enterable e => Change (EditorAttributes e)
editorSendOnCR = editorSend onret
    where
    onret :: String -> Char -> Maybe (Process s -> Process s)
    onret _ k = if k == chr 13 || k == chr 10 then (Yes id) else None

editorSendOnAny :: Enterable e => Change (EditorAttributes e)
editorSendOnAny = editorSend onany
    where
    onany :: String -> Char -> Maybe (Process s -> Process s)
    onany _ _ = Yes id

--An idea that might be used in dialogue boxes...
-- (but can't detect arrow presses at the mo)
--
--notifyOnArrow :: Enterable e => Out Char -> Change (EditorAttributes e)
--notifyOnArrow o = editorSend onarrow
--    where
--    onarrow :: String -> Char -> Maybe (Process s -> Process s)
--    onarrow _ k = if isArrow k then (Yes (tx o k)) else None

editorInput :: Enterable e => In (Change e) -> Change (EditorAttributes e)
editorInput ei (EditorAttributes w h b fgc bgc xc e _ eo o oi s ct) =
    (EditorAttributes w h b fgc bgc xc e ei eo o oi s ct)

editorOutput :: Enterable e => Out (Change e) -> Change (EditorAttributes e)
editorOutput eo (EditorAttributes w h b fgc bgc xc e ei _ o oi s ct) =
    (EditorAttributes w h b fgc bgc xc e ei eo o oi s ct)

editorInit :: Enterable e => e -> Change (EditorAttributes e)
editorInit e (EditorAttributes w h b fgc bgc xc _ ei eo o oi s ct) =
    (EditorAttributes w h b fgc bgc xc e ei eo o oi s ct)

editorCopyThrough :: Enterable e => Change (EditorAttributes e)
editorCopyThrough (EditorAttributes w h b fgc bgc xc e ei eo o oi s _) =
    (EditorAttributes w h b fgc bgc xc e ei eo o oi s True)

--display :: HasOperational (EditorAttributes d) => d -> Gadget
display d = editor' (disabled.editorInit d)

--display' :: HasOperational (EditorAttributes d) => Change (EditorAttributes d) -> d -> Gadget
display' cda d = editor' (disabled.editorInit d.cda)

--enterable :: HasOperational (EditorAttributes e) => e -> Gadget
enterable e = editor' (enabled.editorInit e) 

--enterable' :: HasOperational (EditorAttributes e) => Change (EditorAttributes e) -> e -> Gadget
enterable' cea e = editor' (editorInit e.cea)

editor' :: Enterable e => Change (EditorAttributes e) -> Gadget
editor' cta =
    let (EditorAttributes w h b fgc bgc boc e ei eo o oi sd ct) = 
	    cta (EditorAttributes 100 50 5 fgc' bgc' xc e nci nco True nci s False)
		where
		e = error "initial editor value not set"
		xc = hic
		s _ k = if k == chr 13 || k == chr 10 then (Yes id) else None
	x = w+b+b
	y = h+b+b
	s = toString e 
	--tb :: Enterable e => String -> Bool -> Bool -> e -> Gadget
	tb s o f e =
	    rx [
		from oi $ \o' ->
		    if o' /= o then
			txSM (SMDrawFun (df o' f s)) $
			tb s o' f e
		    else
			tb s o f e,
		from ei $ \de ->
		    let e' = de e in
		    if e' /= e then
			let s' = toString e' in
			when ct (tx eo (const e')) $
			txSM (SMDrawFun (df o f s')) $
			tb s' o f e'
		    else
			tb s o f e,
		fromSM $ \r -> case r of
		    SMMouseClick _ _ _ ->
			if f then
			    txSM SMDisownFocus $
			    txSM (SMDrawFun (df o False s)) $
			    tb s o False e
			else
			    if o then
				txSM SMClaimFocus $
				txSM (SMDrawFun (df o True s)) $
				tb s o True e
			    else
				tb s o f e
		    SMKeyPress k -> 
			if o && f then
			    let (s',c,me) = prokey s k in
			    when c (txSM (SMDrawFun (df o f s'))) $
			    case sd s' k of
				Yes a -> 
				    case me of
					Yes e' -> 
					    a $
					    tx eo (const e') $
					    tb s' o f e'
					None -> 
					    tb s' o f e
				None ->
				    case me of
					Yes e' ->
					    tb s' o f e'
					None ->
					    tb s' o f e
			else
			    tb s o f e
		    SMLoseFocus ->
			txSM (SMDrawFun (df o False s)) $
			tb s o False e
		    otherwise ->
			tb s o f e
	    ] (rxFail "entry")
	(fgc',bgc',flc,shc,lic,hic,foc,enc,dic,c1,c2,c3,c4,c5) = colourset
	df :: Bool -> Bool -> String -> DrawFun
	df o f s (x,y) _ = 
	    let x'=x-1
		y'=y-1
		on c = plinth b shc lic c (0,0) (x',y')++
		    [DrawSetColour bgc,fillbox ((b,b),(x-b-1,y-b-1))]
		off c = plinth b shc lic c (0,0) (x',y')++
		    [DrawSetColour bgc,fillbox ((b,b),(x-b-1,y-b-1))]
	    in if o then
		if f then
		    on boc++[DrawSetColour fgc]++ctext fn (x,y) (s++"_")
		else
		    off enc++[DrawSetColour fgc]++ctext fn (x,y) s
	    else
		off dic++[DrawSetColour fgc]++ctext fn (x,y) s
	fn = "*-lucida-medium-r-*-*-*-120-*-iso8859-1"
    in
    myNameIs "editor" $
    initGadget (x,y) (df o False s) $
    claim ei $
    claim oi $
    tb s o False e
