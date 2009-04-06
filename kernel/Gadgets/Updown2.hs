module Updown2(count) where
import Gadgets

updown2 = (counter,"Counter")

counter :: Gadget
counter =
    myNameIs "counter" $
    wire $ \a ->
    let b1 = button' (picture uparrow) (op a) (+1)
	b2 = button' (picture downarrow) (op a) (+(-1))
	--b2 = combination "123" (picture downarrow) (op b) (+(-1))
	g = bargraph [ip a] in
    wrap' (border 20) (b1 <|> g <|> b2)

updown :: Out Window -> Int -> Gadget
updown o n =
 wire $ \ a ->
 wire $ \ c ->
 wire $ \ d ->
 wire $ \ e ->
 let
  up   = button' (buttonMomentary.picture uparrow) (op a) (+1)
  down = button' (buttonMomentary.picture downarrow) (op a) (+(-1))
  clone = button (op d) ()
  disp = editor' (editorCopyThrough.editorInit n.editorInput (ip a).editorOutput (op c).width 50.height 50)
 in
 spawn (memory (ip d) (ip c) (op e)) $
 spawn (mapC (\f -> let n=f 0 in window (updown o n) "updown") (ip e) o) $
 wrap' (border 30) ((up <-> disp <-> down) <|> clone)

ctr
 = meterWire $ \ a m ->
   let
    u = updown (op a) 0 
    w = wall (ip a)
   in
   wrap (u <-> w <-> m)
    
count = (ctr,"UpDoWn")

up :: Gadget
up =
    wire $ \a ->
    let b = button' (buttonMomentary.picture uparrow) (op a) (+(1::Int))
	d = editor' (disabled.editorInit 0.editorInput (ip a).width 50.height 50)
    in
    wrap' (border 20) (b <-> d)
