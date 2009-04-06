module Os where
import Data.Array((//),(!),listArray)
import Data.Maybe(fromMaybe)
import GadgetsPrelude hiding (Maybe)
import Components
import Sm

os :: Size -> [Window] -> Component
os size ms = posOs size [(Nothing,m)|m<-ms]

posOs :: Size -> [(Maybe Coord,Window)] -> Component
posOs size posms =
    myNameIs "OS" $
    duplex $ \((smrs,smrq),consm) ->
    claim smrs $
    spawn (sm size consm) $

    let newm m c =				-- Create Window (connected to OS and SM)
	    duplex $ \((mosrq,mosrs),osm) ->
	    claim mosrq $

	    wire $ \mlm ->			-- layout connections: move to (out)
	    wire $ \mls ->			-- my size (in)
	    claim (ip mls) $
	    wire $ \mlp ->			-- position me (in)
	    claim (ip mlp) $

            spawnWithState m (WindowState (op mlp) (nco,(ip mlm,op mls),(nci,nco)) osm initGap) $
	    c ((mosrq,mosrs),(ip mls,op mlm),(ip mlp,op mlm)) in

    accumulate (map newm ms) $ \mpins ->		-- Create all Windows

    let (fgc,bgc,flc,shc,lic,hic,foc,enc,dic,c1,c2,c3,c4,c5) = colourset
    in tx smrq (SMMulti [SMMySize size, 
			SMDrawFun (\s b -> [DrawSetColour flc,fillbox b]),
			SMMoveTo (0,0)]) $

    os' (smrs,smrq) (map fst3 mpins) (mls mpins) (mlp mpins) [] ips iss where
    mls mpins = map tagsnd (zip (map snd3 mpins) [1..])
    mlp mpins = map tagsnd (zip (map thrd3 mpins) [1..])
    fst3 (a,_,_) = a
    snd3 (_,a,_) = a
    thrd3 (_,_,a) = a
    tagsnd ((a,b),t) = (a,(b,t))
    os' (smrs,smrq) mosrr mls mlp spins ps ss =
	rx [
	    froms mosrr $ \r _ -> case r of
	    	OSNewCon (ci,co) ->
		    claim ci $
		    os' (smrs,smrq) mosrr mls mlp ((ci,co):spins) ps ss
	    	OSPassToSM p ->
		    tx smrq p $
		    os' (smrs,smrq) mosrr mls mlp spins ps ss,
            froms mls $ \l (mlm,gn) -> case l of
		LOInit s _ dss smcs->
		    let [ds] = dss (ps!gn)
		    in
		    tx smrq (SMAddChild ds smcs) $
		    os' (smrs,smrq) mosrr mls mlp spins ps (ss//[(gn,s)])
		LOSize s _ fa ->
		    tx smrq (SMUpdates (fa (ps!gn))) $
		    os' (smrs,smrq) mosrr mls mlp spins ps (ss//[(gn,s)]),
	    froms mlp $ \(dx,dy) (mlm,gn) ->
		let (ox,oy) = ps!gn
		    (nx,ny) = (ox+dx,oy+dy)
		in
		tx mlm (LOMoveTo (nx,ny)) $
		os' (smrs,smrq) mosrr mls mlp spins (ps//[(gn,(nx,ny))]) ss,
	    froms spins $ \r _ -> case r of
	    	OSNewCon (ci,co) ->
		    claim ci $
		    os' (smrs,smrq) mosrr mls mlp ((ci,co):spins) ps ss
	    	OSPassToSM p ->
		    tx smrq p $
		    os' (smrs,smrq) mosrr mls mlp spins ps ss,
	    from smrs $ \r -> case r of
		_ ->
		    os' (smrs,smrq) mosrr mls mlp spins ps ss 
	] (rxFail "opSystem")
    ips = listArray (1,n)
          [fromMaybe (let a=b*20 in (a,a)) p | (b,p) <- zip [1..n] ps]
    iss = listArray (1,n) (take n (repeat (0,0)))
    n = length ms
    (ps,ms) = unzip posms
