module Calc where
import Gadgets
import Data.Char(isDigit,ord)

type CalcDisplay = Int
type CalcAccumulator = Change Int
type CalcState = (CalcDisplay,CalcAccumulator)
type CalcFunction = Change CalcState

key :: Change ButtonAttributes -> Out CalcFunction -> Char -> Gadget
key cba o c = calcbutton cba c (f c) o
    where
    f '=' (d,a) = (a d, const (a d))
    f 'C' (d,a) = (0,id)
    f c (d,a) | isDigit c = (10*d + ord c - ord '0',a)
    f c (d,a) | otherwise = (0, ((char2op c).a) d)
    char2op '+' = (+)
    char2op '-' = (-)
    char2op '*' = (*)
    char2op '/' = \ x y -> if y/=0 -- a hack to avoid dividing by zero
			   then x `div` y
			   else case compare x 0 of
				  LT -> minBound
				  EQ -> 1
				  GT -> maxBound

calcbutton :: Change ButtonAttributes -> Char -> CalcFunction -> Out CalcFunction -> Gadget
calcbutton cba ch f o =
    let fn = "*lucida-medium-r-*-120-*"
	co = col "black"
    in
    button' (picture (text fn co [ch]).cba) o f

calc = (calc',"Calculator")
calc' :: Gadget
calc' =
    wire $ \n ->
    wire $ \f ->
    let w = 100
	b = 3
	bw = ((w+2*b) `div` 4) - 2 * b
	d = editor' (disabled.border b.width w.height bw.editorInput (ip n).editorInit 0) 
	ks = ["789+","456-","123*","C0=/"]
	k = above (map (beside.map (key (border b.width bw.height bw) (op f))) ks)
	lift :: CalcState -> In CalcFunction -> Out (Change CalcDisplay) -> Component
	lift (d,a) i o =
	    claim i $
	    tx o (const d) $
	    acc' (d,a)
	    where
	    acc' (d,a) =
		rx [
		    from i $ \f ->
			let (d',a') = f (d,a) in
			tx o (const d') $
			acc' (d',a')
		] (rxFail "acc")
    in
    setGapSize 0 $ 
    spawn (lift (0,id) (ip f) (op n)) $
    wrap' (border 20) (d <|> k)
