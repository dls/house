module Updown where
import Gadgets

updown = (counter,"Counter")

counter :: Gadget
counter =
    myNameIs "counter" $
    wire $ \a ->
    let b1 = button' (picture uparrow) (op a) (+1)
	b2 = button' (picture downarrow) (op a) (+(-1))
	--b2 = combination "123" (picture downarrow) (op b) (+(-1))
	g = bargraph [ip a] in
    wrap' (border 20) (b1 <|> g <|> b2)
{-
up :: Gadget
up =
    wire $ \a ->
    let b = button' (buttonMomentary.picture uparrow) (op a) (+1)
	d = editor' (disabled.editorInit 0.editorInput (ip a).width 50.height 50)
    in
    wrap' (border 20) (b <-> d)
-}
