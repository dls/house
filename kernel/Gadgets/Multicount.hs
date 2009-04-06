module Multicount where
import Prelude hiding (sequence)
import Data.Char(chr,ord)
import Gadgets hiding (Maybe)

data CountRq = Manual | Auto | Matic | Copy | Link
type Name = String

countserv :: Out (Maybe Coord,Window) -> Name -> Int -> Bool -> Component
countserv w id n a =
    wire $ \i ->
    let c' os n a s =
	    rx [
		let m a' = 
			let n' = n + 1 in
			sequence [tx o n' | o <- os] $
			c' os n' a' s in
		from (ip i) $ \rq -> case rq of
		    Manual ->
			m False
		    Auto ->
			when (not a) (tx (op i) Matic) $
			c' os n True s
		    Matic ->
			if a then tx (op i) Matic $ m True else c' os n a s
		    Link -> 
			wire $ \o ->
			tx w (Nothing,window (link id n (ip o) (op i)) "link") $
			tx (op o) n $
			c' (op o:os) n a s
		    Copy ->
			let nextId = (chr (s+97):id) in
			spawn (countserv w nextId n a) $
			c' os n a (s+1)
	    ] (rxFail "c") 
    in tx (op i) Link $
    claim (ip i) $
    c' [] n a 0

link :: Name -> Int -> In Int -> Out CountRq -> Gadget
link id n i o =
    typeout ("name "++show id++"\n") $
    wire $ \w ->
    spawn (mapC const i (op w)) $
    let fn = "*lucida*"
	co = col "black"
	d = display' (editorInput (ip w)) n
	a = button' (picture (text fn co "Auto")) o Auto
	m = button' (picture (text fn co "Manual")) o Manual
	c = button' (picture (text fn co "Copy")) o Copy
	l = button' (picture (text fn co "Link")) o Link 
	k = (((length id)*10000)`mod`65536,
		((sum (map ord id))*10000)`mod`65536,
		((sum (map ord id))*13333)`mod`65536) in
    wrap' (border 20.picture (colourbox k)) 
	    (setGapSize 10 ((setGapSize 20 (d <|> a)) <|> m <|> c <|> l))

counters = (counters',"Multiple Counters")
counters' =
    wire $ \w ->
    spawn (countserv (op w) "" 0 False) $
    wall' (width 640.height 480.picture (colourbox (col "light grey"))) (ip w)
