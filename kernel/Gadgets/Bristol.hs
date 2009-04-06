module Bristol where
import Gadgets

bristol = (bristol_challenge,"bristol")

bristol_challenge =
    wire $ \a ->
    wire $ \b ->
    wire $ \c ->
    wire $ \d ->
    wire $ \e ->
    wire $ \f ->
    wire $ \g ->
    wire $ \h ->
    wire $ \i ->
    wire $ \j ->
    wire $ \k ->
    let s = 0::Int
	en = editor' (editorOutput (op a).editorInput (ip k).
		    editorInit s.editorSendOnAny.editorCopyThrough)
	f1 = buttonfunc "fib" fib (ip d) (op c)
	f2 = buttonfunc "id" id (ip e) (op c)
	di = editor' (editorInput (ip i).disabled.editorInit s)
	co = buttonfunc "copy" id (ip h) (op j)
    in 
    spawn (tee (ip f) (op d) (op e)) $
    spawn (tee (ip c) (op g) (op h)) $
    spawn (constToChange (ip j) (op k)) $
    spawn (changeToConst 0 (ip a) (op f)) $
    spawn (constToChange (ip g) (op i)) $
    wrap' (border 20) ((en <|> (f1 <-> f2) <|> di) <.> co )

buttonfunc :: String -> (a -> b) -> In a -> Out b -> Gadget
buttonfunc s f i o =
    wire $ \t ->
    wire $ \m ->
    spawn (memory (ip t) i (op m)) $
    spawn (mapC f (ip m) o) $
    let fn = "*-lucida-medium-r-*-120-*" in
    button' (picture (text fn (col "black") s)) (op t) ()
	
fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)

fib 0 = 1
fib 1 = 1
fib n = fib (n-2) + fib (n-1)
