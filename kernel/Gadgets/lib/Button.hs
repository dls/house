module Button where
import GadgetsPrelude
import Components
import Area(addArea)

-- defines how the state changes (and possibly what the button does) when clicked
type ButtonAction
    = Bool -> SMResponse -> (Bool -> Gadget) -> Gadget

data ButtonAttributes
    = ButtonAttributes Int Int Int DrawFun (In DrawFun) 
			(In Bool) (Out Bool) ButtonAction
			Colour Colour Colour

instance HasWidth ButtonAttributes where
    width w (ButtonAttributes _ h b d di s n a fgc bgc boc) = 
	(ButtonAttributes w h b d di s n a fgc bgc boc)
instance HasHeight ButtonAttributes where
    height h (ButtonAttributes w _ b d di s n a fgc bgc boc) =
	(ButtonAttributes w h b d di s n a fgc bgc boc)
instance HasBorder ButtonAttributes where
    border b (ButtonAttributes w h _ d di s n a fgc bgc boc) =
	(ButtonAttributes w h b d di s n a fgc bgc boc)
instance HasPicture ButtonAttributes where
    picture d (ButtonAttributes w h b _ di s n a fgc bgc boc) =
	(ButtonAttributes w h b d di s n a fgc bgc boc)
instance HasPictureIn ButtonAttributes where
    pictureIn di (ButtonAttributes w h b d _ s n a fgc bgc boc) =
	(ButtonAttributes w h b d di s n a fgc bgc boc)
instance HasFGColour ButtonAttributes where
    fgcol fgc (ButtonAttributes w h b d di s n a _ bgc boc) =
	(ButtonAttributes w h b d di s n a fgc bgc boc)
instance HasBGColour ButtonAttributes where
    bgcol bgc (ButtonAttributes w h b d di s n a fgc _ boc) =
	(ButtonAttributes w h b d di s n a fgc bgc boc)
instance HasXColour ButtonAttributes where
    xcol boc (ButtonAttributes w h b d di s n a fgc bgc _) =
	(ButtonAttributes w h b d di s n a fgc bgc boc)
instance HasOperational ButtonAttributes where
    operational False = buttonAction ca
	where
	ca s _ c = c s
    operational True = id

buttonNotify :: Out Bool -> ButtonAttributes -> ButtonAttributes
buttonNotify n (ButtonAttributes w h b d di s _ a fgc bgc boc) = (ButtonAttributes w h b d di s n a fgc bgc boc)

buttonSet :: In Bool -> ButtonAttributes -> ButtonAttributes
buttonSet s (ButtonAttributes w h b d di _ n a fgc bgc boc) = (ButtonAttributes w h b d di s n a fgc bgc boc)

buttonAction :: ButtonAction -> ButtonAttributes -> ButtonAttributes
buttonAction a (ButtonAttributes w h b d di s n _ fgc bgc boc) = (ButtonAttributes w h b d di s n a fgc bgc boc)

buttonPonPoff :: ButtonAttributes -> ButtonAttributes
buttonPonPoff = buttonAction ca
    where
    ca False (SMMouseClick _ _ _) c = c True
    ca True (SMMouseClick _ _ _) c = c False
    ca s _ c = c s

buttonPon :: ButtonAttributes -> ButtonAttributes
buttonPon = buttonAction ca
    where
    ca False (SMMouseClick _ _ _) c = c True
    ca s _ c = c s

buttonMomentary :: ButtonAttributes -> ButtonAttributes
buttonMomentary = buttonAction ca
    where
    ca False (SMMouseClick _ _ _) c = c True
    ca True (SMMouseClick _ _ _) c = c True
    ca False (SMMouseUnClick _ _ _) c = c False
    ca True (SMMouseUnClick _ _ _) c = c False
    ca s _ c = c s

button = button' id

button' :: Change ButtonAttributes -> Out m -> m -> Gadget
button' cba mo m =
    let (fgc',bgc',flc,shc,lic,hic,foc,enc,dic,c1,c2,c3,c4,c5) = colourset
	(ButtonAttributes sx sy g df ndf ss no a fgc bgc boc) = 
	    (cba.buttonMomentary) (ButtonAttributes 50 50 5 transparent nci nci nco da fgc' bgc' enc)
		where 
		da b _ c = c b
	newpic p c = 
	    txSM (SMDrawFun p) $ 
	    c
	updpic u p c = 
	    txSM (SMUpdateDrawFun u p) $ 
	    c
	(asx,asy) = (sx+g+g,sy+g+g)
	butdf d (x,y) r = [DrawSetColour bgc,fillbox ((g,g),(x-g-1,y-g-1))]++
			    moveOrigin (g,g) (d (x-g-g,y-g-g) (addArea (-g,-g) r))
	butOn (x,y) _ = plinth g shc lic hic (0,0) (x-1,y-1)
	butOnF d (x,y) r = butdf d (x,y) r++butOn (x,y) r
	butOff (x,y) r = plinth g shc lic enc (0,0) (x-1,y-1)
	butOffF d (x,y) r = butdf d (x,y) r++butOff (x,y) r
    in
    myNameIs "button" $
    claim ndf $
    claim ss $
    let but s df =
	    let but' = but
		click c = case c of
		    SMMouseClick _ _ 2 ->
			typeout "larger\n" $
			setSize (100,100) $
			but' s df
		    SMMouseClick _ _ 3 ->
			typeout "smaller\n" $
			setSize (60,60) $
			but' s df
		    otherwise ->
			a s c $ \s' ->
			if s' /= s then
			    tx no s' $
			    when s' (tx mo m) $
			    when (g>2) ((if s' then updpic butOn (butOnF df) 
						else updpic butOff (butOffF df))) $
			    but' s' df
			else
			    but' s' df
	    in
	    rx [
		from ss $ \s' ->
		    if s' /= s then
			when (g>2) ((if s' then updpic butOn (butOnF df) 
					    else updpic butOff (butOffF df))) $
			when s' (tx mo m) $
			but' s' df
		    else
			but' s' df,
		from ndf $ \df' ->
		    updpic (butdf df') (if s then butOnF df' else butOffF df') $
		    but' s df',
		fromSM $
		    click
	    ] (rxFail "button")
    in
    initGadget (asx,asy) (butOffF df) $
    but False df
