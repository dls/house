module Instruments where
import GadgetsPrelude
import Components
import EditorG
import Display

meterWire :: ComponentClass s => (Wire a -> Gadget -> Process s) -> Process s
meterWire c =
    wire $ \i ->
    wire $ \o ->
    wire $ \d ->
    let monitor :: In a -> Out a -> Out (Change Int) -> Component
	monitor i o d =
	    claim i $
	    mc where
		mc =
		    rx [
			from i $ \m ->
			    tx d (+1) $
			    tx o m $
			    mc
		    ] (rxFail "meterWire monitor")
    in
    spawn (monitor (ip i) (op o) (op d)) $
    c (ip o,op i) (display' (bgcol (col "yellow").border 0.disabled.
		    height 20.width 50.editorInput (ip d)) 0)

meterIn :: ComponentClass s => In a -> (In a -> Gadget -> Process s) -> Process s
meterIn i c =
    wire $ \o ->
    wire $ \d ->
    let monitor :: In a -> Out a -> Out (Change Int) -> Component
	monitor i o d =
	    claim i $
	    mc where
		mc =
		    rx [
			from i $ \m ->
			    tx d (+1) $
			    tx o m $
			    mc
		    ] (rxFail "meterWire monitor")
    in
    spawn (monitor i (op o) (op d)) $
    c (ip o) (display' (bgcol (col "red").border 0.disabled.
		    height 20.width 50.editorInput (ip d)) 0)

meterOut :: ComponentClass s => Out a -> (Out a -> Gadget -> Process s) -> Process s
meterOut o c =
    wire $ \i ->
    wire $ \d ->
    let monitor :: In a -> Out a -> Out (Change Int) -> Component
	monitor i o d =
	    claim i $
	    mc where
		mc =
		    rx [
			from i $ \m ->
			    tx d (+1) $
			    tx o m $
			    mc
		    ] (rxFail "meterWire monitor")
    in
    spawn (monitor (ip i) o (op d)) $
    c (op i) (display' (bgcol (col "green").border 0.disabled.
		    height 20.width 50.editorInput (ip d)) 0)
