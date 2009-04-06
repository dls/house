module Wall where
import GadgetsPrelude hiding (Maybe)
import Components

data Wall = Wall Size DrawFun Coord

instance HasPicture Wall where
    picture df (Wall s _ p) = Wall s df p
instance HasWidth Wall where
    width w (Wall (_,h) df p) = Wall (w,h) df p
instance HasHeight Wall where
    height h (Wall (w,_) df p) = Wall (w,h) df p

childrenAt p (Wall s df _) = Wall s df p

wall = wall' id

wall' :: Change Wall -> In (Maybe Coord,Window) -> Gadget
wall' cwa i =
    claim i $
    initGadget (sx,sy) df $
    wloop childPos [] [] [] []
    where
    brickwall (x,y) _ = [colour "red",fillbox ((0,0),(x-1,y-1))]++
	[colour "black"]++[drawline ((0,u),(x,u)) | u <- [0,20..y]]++
	[drawline ((a,v),(a,v+20)) | a <- [0,40..x], v <- [0,40..y]]++
	[drawline ((a,v),(a,v+20)) | a <- [20,60..x], v <- [20,60..y]]
    Wall (sx,sy) df childPos = cwa (Wall (500,500) brickwall (10,10))
    wloop :: Coord -> [(Out LOResponse,In LORequest,In Coord)] -> [Coord] -> [Size] -> 
		[Coord -> [DisplayChange]] -> Gadget
    wloop (x,y) ws ps ss mf =
	let fws = zip (map (\(mt,ms,mp) -> ms) ws) [0..]
	    fwp = zip (map (\(mt,ms,mp) -> mp) ws) [0..]
	in
	rx [
	    from i $ \ (optPos,w) ->
		wire $ \wmt ->
		wire $ \wms ->
		claim (ip wms) $
		wire $ \wmp ->
		claim (ip wmp) $
		duplex $ \(wos,osw) ->
		txOS (OSNewCon osw) $
		spawnWithState w (WindowState (op wmp) (nco,(ip wmt,op wms),(nci,nco)) wos initGap) $
                let (pos,next) =
		      case optPos of
                        Nothing -> ((x,y),((x+20)`mod`sx,(y+20)`mod`sy))
			Just p  -> (p,(x,y))
		in wloop next ((op wmt,ip wms,ip wmp):ws) (pos:ps) ((0,0):ss) ((\_->[]):mf),
	    froms fws $ \l gn -> case l of
		LOSize s fm fo ->
		    txSM (SMUpdates (fo (ps!!gn))) $
		    wloop (x,y) ws ps (replace gn s ss) (replace gn fm mf)
		LOInit s fm dss smcs -> 
		    let [ds] = dss (ps!!gn) in
		    txSM (SMAddChild ds smcs) $
		    wloop (x,y) ws ps (replace gn s ss) (replace gn fm mf),
	    froms fwp $ \(dx,dy) gn ->
		let (ox,oy) = ps!!gn
		    (nx,ny) = (ox+dx,oy+dy)
		in
		txSM (SMUpdates ((mf!!gn) (nx,ny))) $
		wloop (x,y) ws (replace gn (nx,ny) ps) ss mf,
	    fromSM $ \_ -> wloop (x,y) ws ps ss mf
	] (rxFail "wall")
    replace :: Int -> a -> [a] -> [a]
    replace 0 a (b:bs) = a:bs
    replace n a (b:bs) = b:(replace (n-1) a bs)
