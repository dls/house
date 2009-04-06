module Radiogroup where
import Prelude hiding (sequence)
import GadgetsPrelude
import Components
import Button

radioGroup :: ComponentClass c => [InOut Bool Bool -> g] -> ([g] -> Process c) -> Process c
radioGroup [g] _ = error "Can't make a radio group out of one item"
radioGroup gs c = rg [] [] gs
    where
    rg a cg [] = 
	spawn (radioControl a) $
	c cg
    rg a cg (g:gs) =
	duplex $ \(ge,ce) ->
	let g' = g ge in
	rg (ce:a) (g':cg) gs

radioControl :: [InOut Bool Bool] -> Component
radioControl c =
    sequence (map (claim.fst) c) $
    set ((snd.head) c) $
    rc ((snd.head) c) where
    rc s =
	rx [
	    froms c $ \t r -> case t of
		True -> 
		    reset s $
		    rc r
		False -> 
		    rc s
	] (rxFail "radiocontrol")
    set o = tx o True
    reset o = tx o False

--radioGate :: ComponentClass c => In i -> Out o -> [InOut o i -> g] -> ([g] -> Process c) -> Process c
radioGate _ _ [g] _ = error "Can't make radioGate with one item"
radioGate i o gs c = rg [] [] gs
    where
    rg a cg [] =
	spawn (radioGateControl i o a) $
	c cg
    rg a cg (g:gs) =
	duplex $ \(ge,ce) ->
	let g' = g ge in
	rg (ce:a) (g':cg) gs

data GateState m = GateNone | GateMsg [m] | GateOpen (Out m)

radioGateControl :: In i -> Out o -> [InOut o i] -> Component
radioGateControl i o cs =
    claim i $
    sequence (map (claim.fst) cs) $
    r GateNone where
    r c =
	rx [
	    from i $ \m ->
		case c of
		    GateNone ->
			r (GateMsg [m])
		    GateMsg om ->
			r (GateMsg (m:om))
		    GateOpen t ->
			tx t m $
			r c,
	    froms cs $ \m t ->
		tx o m $
		case c of
		    GateNone ->
			r (GateOpen t)
		    GateMsg ms ->
			sequence (map (tx t) ms) $
			r (GateOpen t)
		    GateOpen _ ->
			r (GateOpen t)
	] (rxFail "radioGateControl")

radioButton :: InOut Bool Bool -> Change ButtonAttributes -> Out m -> m -> Gadget
radioButton (se,cl) cba mo m = button' (buttonPon.buttonSet se.buttonNotify cl.cba) mo m

radioButtons :: Int -> ([Change ButtonAttributes -> Out m -> m -> Gadget] -> Gadget) -> Gadget
radioButtons n c =
    let bs = replicate n radioButton in
    radioGroup bs c
