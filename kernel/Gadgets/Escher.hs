module Escher where
import Prelude hiding (sequence)
import Gadgets

rotateCW :: DrawFun -> DrawFun
rotateCW df =
    let sc = dfToSc (10000,10000) df
	sc' = map ro sc
	ro (DrawLine ((x1,y1),(x2,y2))) = DrawLine ((10000-y1,x1),(10000-y2,x2))
	ro (DrawFilledRectangle ((x1,y1),(x2,y2))) = DrawFilledRectangle ((10000-y1,x1),(10000-y2,x2))
	ro (DrawSetColour c) = DrawSetColour c in
    scToDf (10000,10000) sc'

rotateACW :: DrawFun -> DrawFun
rotateACW df =
    let sc = dfToSc (10000,10000) df
	sc' = map ro sc
	ro (DrawLine ((x1,y1),(x2,y2))) = DrawLine ((y1,10000-x1),(y2,10000-x2))
	ro (DrawFilledRectangle ((x1,y1),(x2,y2))) = DrawFilledRectangle ((y1,10000-x1),(y2,10000-x2))
	ro (DrawSetColour c) = DrawSetColour c in
    scToDf (10000,10000) sc'

mirrorH :: DrawFun -> DrawFun
mirrorH df =
    let sc = dfToSc (10000,10000) df
	sc' = map mi sc
	mi (DrawLine ((x1,y1),(x2,y2))) = DrawLine ((10000-x1,y1),(10000-x2,y2))
	mi (DrawFilledRectangle ((x1,y1),(x2,y2))) = DrawFilledRectangle ((10000-x1,y1),(10000-x2,y2))
	mi (DrawSetColour c) = DrawSetColour c in
    scToDf (10000,10000) sc'

mirrorV :: DrawFun -> DrawFun
mirrorV df =
    let sc = dfToSc (10000,10000) df
	sc' = map mi sc
	mi (DrawLine ((x1,y1),(x2,y2))) = DrawLine ((x1,10000-y1),(x2,10000-y2))
	mi (DrawFilledRectangle ((x1,y1),(x2,y2))) = DrawFilledRectangle ((x1,10000-y1),(x2,10000-y2))
	mi (DrawSetColour c) = DrawSetColour c in
    scToDf (10000,10000) sc'
design :: In DrawFun -> Out DrawFun -> Gadget
design i o =
    wire $ \d ->
    wire $ \b ->
    spawn (memory (ip b) (ip d) o) $
    let p = paper' (gridlock (20,20).width 400.height 400.pictureIn i.pictureOut (op d))
	s = button' (picture transicon) (op b) ()
    in
    wrap (p <-> s)

transicon (x',y') _ =
    let (x,y) = (x'-1,y'-1)
	sx q d = ((q * x') `div` d) - 1
	sy q d = ((q * y') `div` d) - 1 in
    [colour "black", drawline ((sx 1 5,sy 1 3),(sx 1 2,sy 1 3)),
    drawline ((sx 1 2,sy 1 3),(sx 1 2,sy 1 6)), drawline ((sx 1 2,sy 1 6),(sx 4 5,sy 1 2)),
    drawline ((sx 4 5,sy 1 2),(sx 1 2,sy 5 6)), drawline ((sx 1 2,sy 5 6),(sx 1 2,sy 2 3)),
    drawline ((sx 1 2,sy 2 3),(sx 1 5,sy 2 3)), drawline ((sx 1 5,sy 2 3),(sx 1 5,sy 1 3))]

--designtest = go [(wrap (wire $ \w ->((design nci (op w))<->(button' (pictureIn (ip w)) nco ()))),"designtest")]

pictureStoreButton :: InOut Bool Bool -> InOut DrawFun () -> Out DrawFun -> Gadget
pictureStoreButton (se,no) (pi,cl) po =
    wire $ \a ->
    wire $ \b ->
    wire $ \c ->
    wire $ \d ->
    wire $ \e ->
    spawn (tee pi (op a) (op b)) $
    spawn (buffer [transparent] (ip a) (op e)) $
    spawn (memory (ip d) (ip e) po) $
    spawn (tee (ip c) (op d) cl) $
    button' (buttonPon.pictureIn (ip b).buttonSet se.buttonNotify no) (op c) () 

choice :: Int -> In DrawFun -> Out DrawFun -> Gadget
choice n i o =
    let ucb = replicate n pictureStoreButton in
    radioGroup ucb $ \rb ->
    radioGate i nco rb $ \cb ->
    let cb' = map (\b->b o) cb in
    wrap (above cb')

tileTool :: (DrawFun->DrawFun) -> In DrawFun -> InOut Bool Bool -> Out (DrawFun->DrawFun) -> Gadget
tileTool f i (se,cl) o =
    wire $ \a ->
    wire $ \b ->
    wire $ \c ->
    wire $ \d ->
    wire $ \e ->
    spawn (mapC f i (op a)) $
    spawn (tee (ip a) (op b) (op e)) $
    spawn (mapC (\d->(\_->d)) (ip b) (op c)) $
    spawn (memory (ip d) (ip c) o) $
    button' (buttonSet se.buttonNotify cl.pictureIn (ip e).buttonPon) (op d) ()

tileTools = [tileTool id, tileTool rotateCW, tileTool (rotateCW.rotateCW), tileTool rotateACW,
    tileTool mirrorV, tileTool (rotateCW.mirrorV), tileTool (rotateCW.rotateCW.mirrorV),
    tileTool (rotateACW.mirrorV)]

fixedTool :: DrawFun -> (DrawFun->DrawFun) -> InOut Bool Bool -> Out (DrawFun->DrawFun) -> Gadget
fixedTool p f (se,cl) o = 
    button' (buttonSet se.buttonNotify cl.picture p.buttonPon) o f

fixedTools = [fixedTool rotCWIcon rotateCW, fixedTool rotACWIcon rotateACW, fixedTool mirHIcon mirrorH, fixedTool mirVIcon mirrorV]

rotCWIcon = scToDf (10000,10000) [DrawSetColour (0,0,0),DrawLine ((8000,2000),(8000,4000)),DrawLine ((6000,4000),(8000,4000)),DrawLine ((6000,8000),(4000,8000)),DrawLine ((4000,8000),(2000,6000)),DrawLine ((2000,6000),(2000,4000)),DrawLine ((2000,4000),(4000,2000)),DrawLine ((4000,2000),(6000,2000)),DrawLine ((6000,2000),(8000,4000))]

rotACWIcon = mirrorH rotCWIcon

mirVIcon = scToDf (10000,10000) [DrawSetColour (0,0,0),DrawLine ((7000,4000),(5000,2000)),DrawLine ((3000,4000),(5000,2000)),DrawLine ((3000,6000),(5000,8000)),DrawLine ((7000,6000),(5000,8000)),DrawLine ((5000,8000),(5000,2000))]

mirHIcon = rotateCW mirVIcon

tools :: In DrawFun -> Out (DrawFun -> DrawFun) -> Gadget
tools i o =
    wiresIn tileTools $ \tt bi ->
    spawn (broadcast i bi) $
    radioGroup (tt++fixedTools) $ \rt ->
    let gs = map (\g->g o) rt in
    wrap (beside gs)

board :: (Int,Int) -> In (DrawFun->DrawFun) -> Gadget
board (sx,sy) i =
    let bcs = replicate (sx*sy) boardcell in
    wiresInOut bcs $ \bs ws ->
    setGapSize 0 $
    spawn (boardControl i ws) $
    let g' [] = []
	g' gs = let (h,t) = splitAt sx gs in h:g' t
	p = g' bs
	(fgc,bgc,flc,shc,lic,hic,foc,enc,dic,c1,c2,c3,c4,c5) = colourset
	bg (x,y) _ = plinth 5 shc lic enc (0,0) (x-1,y-1)++
	    [DrawSetColour bgc,fillbox ((5,5),(x-6,y-6))]
    in wrap' (border 5.picture bg) (above (map beside p))

data BoardControlState 
    = BoardNone 
    | BoardFun (DrawFun->DrawFun) 
    | BoardWaiting [Out (DrawFun->DrawFun)]

boardControl :: In (DrawFun->DrawFun) -> [InOut () (DrawFun->DrawFun)] -> Component
boardControl f bcs =
    myNameIs "boardControl" $
    claim f $
    sequence (map (claim.fst) bcs) $
    bc BoardNone where
    bc s =
	rx [
	    from f $ \ddf' ->
		case s of
		    BoardWaiting rs ->
			sequence (map (flip tx ddf') rs) $
			bc (BoardFun ddf')
		    otherwise ->
			bc (BoardFun ddf'),
	    froms bcs $ \_ r ->
		case s of
		    BoardNone ->
			bc (BoardWaiting [r])
		    BoardFun ddf ->
			tx r ddf $
			bc s
		    BoardWaiting rs ->
			bc (BoardWaiting (r:rs))
	] (rxFail "boardControl")

boardcell :: InOut (DrawFun->DrawFun) () -> Gadget
boardcell (i,o) =
    wire $ \a ->
    wire $ \b ->
    wire $ \c ->
    wire $ \d ->
    spawn (apply i (ip a) (op b)) $
    spawn (buffer [transparent] (ip c) (op a)) $
    spawn (tee (ip b) (op c) (op d)) $
    button' (pictureIn (ip d).border 0.buttonMomentary) o ()

escher = (escher_prog,"Escher")

escher_prog :: Gadget
escher_prog =
    wire $ \dc ->
    wire $ \ct ->
    wire $ \td ->
    wire $ \tt ->
    wire $ \tb ->
    setGapSize 20 $
    spawn (tee (ip ct) (op td) (op tt)) $
    let d = design (ip td) (op dc)
	c = choice 5 (ip dc) (op ct) 
	t = tools (ip tt) (op tb)
	b = board (6,6) (ip tb)
	in
    wrap' (border 20) (d <-> c <-> b <|> t)
