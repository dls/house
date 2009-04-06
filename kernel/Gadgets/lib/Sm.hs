module Sm where
import Prelude hiding (Maybe(..),sequence)
import GadgetsPrelude
import Components
import Display

-- S C R E E N   M A N A G E R   ( S M )

-- screen manager state:

type SMState = (
    Bool,			       -- 1. has first expose event arrived yet?
    DisplayPlus,		       -- 2. display data structure
    Maybe (ImageID,Coord,ClickAction), -- 3. ID of image that will receive next MouseUnClick
				       --    with coord of image origin, and action to perform for
				       --    corresponding unclick.
    [InOut SMRequest SMResponse],      -- 4. list of SM rq/rs duplexpins
    Maybe ImageID)		       -- 5. ID of image with input focus

defClickAct o (x,y) b c = tx o (SMMouseClick x y b) $ c
defUnClickAct o (x,y) b c = tx o (SMMouseUnClick x y b) $ c

sm :: Size -> (In SMRequest,Out SMResponse) -> Component
sm size (i,o) =
    myNameIs "SM" $
    claim i $

    wire $ \sms ->				-- SM to Screen
    wire $ \ssm ->				-- Screen to SM
    claim (ip ssm) $
    spawn (screen (ip sms) (op ssm)) $		-- spawn screen

    wire $ \smm ->				-- SM to Mouse
    wire $ \msm ->				-- Mouse to SM
    claim (ip msm) $
    spawn (mouse (ip smm) (op msm)) $		-- spawn mouse

    wire $ \smk ->				-- SM to Keyboard
    wire $ \ksm ->				-- Keyboard to SM
    claim (ip ksm) $
    spawn (keyboard (ip smk) (op ksm)) $	-- spawn keyboard

    let initState :: SMState
	initState = (False, firstImage size o transparent True (defClickAct o) (defUnClickAct o),
			None, [(i,o)], None) in

    sml ((ip ssm,op sms,ip msm,op smm,ip ksm,op smk),initState) where
    sml (pc@(screenEv,screenCo,mouseEv,mouseCo,keyboardEv,keyboardCo),
	    st@(fe,display,uncl,smrr,fo)) =
	let sm' ns = sml (pc,ns)
	    service smrq smrs st@(fe,display,uncl,smrr,fo) continue = 
		case smrq of
		    SMAddChild ch cos ->			-- new child request
			debugsm ("AddChild "++show smrs++"\n") $
			let display' = cacheUpdate (addChild smrs ch) display
			in
			sequence (map (claim.fst) cos) $
			continue (fe,display',uncl,cos++smrr,fo)
		    SMNewCon (ci,co) ->				-- new connection request
			debugsm ("NewCon "++show smrs++" ("++show co++")\n") $
			let a = mkImage co ((0,0),(0,0)) transparent True 
				    (defClickAct co) (defUnClickAct co) [] True
			    display' = cacheUpdate (addChild smrs a) display 
			in
			claim ci $
			continue (fe,display',uncl,(ci,co):smrr,fo)
		    SMMoveTo pos ->				-- move image
			debugsm ("MoveTo "++show smrs++"\n") $
			let display' = cacheUpdate (moveImage smrs pos) display 
			in
			continue (fe,display',uncl,smrr,fo)
		    SMMySize size ->				-- change image size
			debugsm ("MySize "++show smrs++"\n") $
			let display' = cacheUpdate (resizeImage smrs size) display 
			in
			continue (fe,display',uncl,smrr,fo)
		    SMDrawFun df ->				-- change image draw function
			debugsm ("DrawFun "++show smrs++"\n") $
			let display' = cacheUpdate (newImageDrawFun smrs df) display 
			in
			continue (fe,display',uncl,smrr,fo)
		    SMUpdateDrawFun ud df ->			-- update image draw function
			debugsm ("UpdateDrawFun "++show smrs++"\n") $
			let display' = cacheUpdate (newImageDrawFun smrs ud) display 
			    (suc,display'') = update_and_redraw display'
			    display''' = cacheUpdate (quietImageDrawFun smrs df) display''
			in
			tx screenCo suc $
			continue (fe,display''',uncl,smrr,fo)
		    SMInvisible ->				-- make image invisible
			debugsm ("Invisible "++show smrs++"\n") $
			let display' = cacheUpdate (invisibleImage smrs) display
			in
			continue (fe,display',uncl,smrr,fo)
		    SMVisible ->				-- make image visible
			typeout ("Visible "++show smrs++"\n") $
			let display' = cacheUpdate (visibleImage smrs) display
			in
			continue (fe,display',uncl,smrr,fo)
		    SMClickAction ca ->				-- change image click action
			let display' = cacheUpdate (imageClickAction smrs ca) display
			in
			continue (fe,display',uncl,smrr,fo)
		    SMUnClickAction uca ->			-- change image unclick action
			let display' = cacheUpdate (imageUnClickAction smrs uca) display
			in
			continue (fe,display',uncl,smrr,fo)
		    SMDumpInfo ->				-- dump sm display information
			typeout "\nScreen Manager Display data structure dump:\n" $
			typeout (show display) $
			typeout "\n" $
			continue st
		    SMRedraw ->					-- redraw display (redundant)
			typeout ("Redraw "++show smrs++"\n") $
			continue st
		    SMDoAction a ->				-- perform the given action
			a $
			continue st
		    SMMulti g ->
			debugsm ("Multi "++show smrs++"\n") $
			let f [] st = continue st
			    f (g:gs) st = service g smrs st $ \st' -> f gs st'
			in f g st
		    SMFor f r ->
			-- NB. if SMFor msgs are sent inside SMMulti then they must come
			-- in pre-order.
			debugsm ("For "++show smrs++" ("++show f++")\n") $
			service r f st $ \st' -> continue st'
		    SMClaimFocus ->
			debugsm ("ClaimFocus "++show smrs++"\n") $
			case fo of
			    None -> 
				continue (fe,display,uncl,smrr,Yes smrs)
			    Yes ofo ->
				tx ofo SMLoseFocus $
				continue (fe,display,uncl,smrr,Yes smrs)
		    SMDisownFocus ->
			debugsm ("DisownFocus "++show smrs++"\n") $
			if fo == Yes smrs then
			    continue (fe,display,uncl,smrr,None)
			else
			    continue st
		    SMUpdates u ->
			debugsm ("Updates "++show smrs++" ("++show (map fst u)++")\n") $
			let display' = cacheUpdates u display in
			continue (fe,display',uncl,smrr,fo)
	in
	rx [
	    froms smrr $ \smrq smrs ->			-- ****** SM REQUEST
		service smrq smrs st $ \st'@(fe',display',uncl',smrr',fo') ->
		if fe' then
		    debugsm ("Service redraw "++show smrs++"\n") $
		    debugsm ("update\n") $
		    let (suc,display'') = update_and_redraw display'
		    in
		    tx screenCo suc $
		    sm' (fe',display'',uncl',smrr',fo')
		else
		    debugsm ("Service no redraw "++show smrs++"\n") $
		    debugsm ("update\n") $
		    let display'' = update display'
		    in
		    sm' (fe',display'',uncl',smrr',fo'),
	    from mouseEv $ \e -> case e of		-- ****** MOUSE EVENT
		MouseClick x y b -> 		-- mouse click
		    debugsm "Click\n" $
		    case click_dest display (x,y) of	
		        None ->
			    sm' st
			Yes (wrp,(x1,y1),(cx,cy),(ca,uca)) ->
			    ca (cx,cy) b $
			    sm' (fe,display,Yes (wrp,(x1,y1),uca),smrr,fo)
		MouseUnClick x y b -> 
		    debugsm "Unclick\n" $
		    case uncl of		-- mouse click
		    None ->
		        sm' (fe,display,None,smrr,fo)
		    Yes (wrp,(ox,oy),uca) ->
		        uca (x-ox,y-oy) b $
		        sm' (fe,display,None,smrr,fo),
	    from screenEv $ \e -> case e of		-- ****** SCREEN EVENT
		Expose x ->
		    --debugsm "\nScreen Manager Display data structure dump:\n" $
		    --debugsm (show display) $
		    --debugsm "\n" $
		    debugsm "Expose\n" $
		    let display' = needs_redrawing x display
			(suc,display'') = update_and_redraw display'
		    in 
		    tx screenCo suc $
		    --debugsm "\nScreen Manager Display data structure dump:\n" $
		    --debugsm (show display') $
		    --debugsm "\n" $
		    sm' (True,display'',uncl,smrr,fo),
	    from keyboardEv $ \e -> case e of
		KeyPress k ->
		    case fo of
			Yes foi ->
			    tx foi (SMKeyPress k) $
			    sm' st
			None ->
			    case k of
				'd' ->
				    typeout "\nScreen Manager Display data structure dump:\n" $
				    typeout (show display) $
				    typeout "\n" $
				    sm' st
				'u' ->
				    let display' = update display in
				    typeout "\nScreen Manager Display data structure dump:\n" $
				    typeout (show display') $
				    typeout "\n" $
				    sm' (fe,display',uncl,smrr,fo)
				'r' ->
				    let (suc,display') = redraw display in
				    tx screenCo suc $
				    sm' (fe,display',uncl,smrr,fo)
				'b' ->
				    let (suc,display') = update_and_redraw display in
				    typeout "\nScreen Manager Display data structure dump:\n" $
				    typeout (show display') $
				    typeout "\n" $
				    tx screenCo suc $
				    sm' (fe,display',uncl,smrr,fo)
				otherwise ->
				    typeout "(bleep)\n" $
				    sm' st
	] (rxFail "screenMan")
    --debugsm = typeout
    debugsm _ c = c
