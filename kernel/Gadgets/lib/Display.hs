module Display where
import Prelude hiding (Maybe(..))
import GadgetsPrelude
import Area

--scrDimen = ((0,0),(maxXCoord, maxYCoord))

click_dest :: DisplayPlus -> Coord -> Maybe (ImageID,Coord,Coord,(ClickAction,ClickAction))
click_dest (DisplayPlus size display _ _) (x,y) = cd (0,0) ((0,0),size) display
    where
    cd :: Coord -> Area -> Display -> Maybe (ImageID,Coord,Coord,(ClickAction,ClickAction))
    cd _ _ (Image _ (ImageInfo _ _ False _ _ _) _ _) = None
    cd (xo,yo) ((l,t),(r,b)) (Image imid (ImageInfo ((x1,y1),(x2,y2)) _ _ ca uca _) children _) =
	let ax1 = xo+x1		-- absolute position of Image
	    ay1 = yo+y1
            ax2 = xo+x2
            ay2 = yo+y2
            bx1 = max ax1 l	-- Image clipped to parent's borders
            by1 = max ay1 t
            bx2 = min ax2 r
            by2 = min ay2 b in
        if bx1<=x && x<bx2 && by1<=y && y<by2 then
            case (filter isYes (map (cd (ax1,ay1) ((bx1,by1),(bx2,by2))) children)) of
		(h:_) -> h
                otherwise -> Yes (imid,(ax1,ay1),(x-ax1,y-ay1),(ca,uca))
        else
            None

update_click_dest :: DisplayPlus -> Coord -> Maybe (ImageID,Coord,Coord,(ClickAction,ClickAction))
update_click_dest dp@(DisplayPlus _ _ _ []) click = click_dest dp click
update_click_dest dp click = click_dest (update dp) click

needs_redrawing :: Area -> DisplayPlus -> DisplayPlus
needs_redrawing a (DisplayPlus size display nr changes) = 
    DisplayPlus size display (unionArea a nr) changes

redraw :: DisplayPlus -> ([ScreenCmnd],DisplayPlus)
redraw dp@(DisplayPlus size display nr changes) = (concat sc,DisplayPlus size display [] changes)
    where
    (sc,_) = rd (0,0) [] nr display
    rd :: Coord -> [[ScreenCmnd]] -> [Area] -> Display -> ([[ScreenCmnd]],[Area])
    rd i_origin i_scrcmds i_redraw (Image imid (ImageInfo rpos df vis ca uca size) children ov) =
	let apos = addArea i_origin rpos
	    clipped = if vis then intersectArea apos i_redraw else []
	    origin@(ox,oy) = fst apos

	    (scrcmds',redraw') = 
		if ov then
		    fov i_scrcmds clipped children
		else
		    fdi i_scrcmds clipped children
		where
		fdi :: [[ScreenCmnd]] -> [Area] -> [Display] -> ([[ScreenCmnd]],[Area])
		fdi i_scrcmds i_redraw [] = (i_scrcmds,i_redraw)
		fdi i_scrcmds i_redraw (c:cs) = fdi s_scrcmds s_redraw cs
		    where
		    (s_scrcmds,s_covers) = rd origin i_scrcmds clipped c
		    s_redraw = subtractAreas s_covers i_redraw

		fov :: [[ScreenCmnd]] -> [Area] -> [Display] -> ([[ScreenCmnd]],[Area])
		fov i_scrcmds i_redraw [] = (i_scrcmds,i_redraw)
		fov i_scrcmds i_redraw (c:cs) = fov s_scrcmds s_redraw cs
		    where
		    (s_scrcmds,s_covers) = rd origin i_scrcmds i_redraw c
		    s_redraw = subtractAreas s_covers i_redraw
	in
	    if (not.null) clipped then
		let generate a = --trace ("DRAW "++show imid++" "++show a++"\n")
				--(((DrawSetClip a):(colour "black"):(copy 200 (fillbox a)))++
				((DrawSetClip a):moveOrigin origin (df size (addArea (-ox,-oy) a)))
				    --)
		    f :: [[ScreenCmnd]] -> [Area] -> [[ScreenCmnd]]
		    f i_scrcmds [] = i_scrcmds
		    f i_scrcmds (r:rs) = f ((generate r):i_scrcmds) rs
		in (f scrcmds' redraw',clipped)
	    else
		(i_scrcmds,[])

update_and_redraw :: DisplayPlus -> ([ScreenCmnd],DisplayPlus)
update_and_redraw dp@(DisplayPlus _ _ [] []) = ([],dp)
update_and_redraw dp@(DisplayPlus _ _ _  []) = redraw dp
update_and_redraw dp = redraw (update dp)

update :: DisplayPlus -> DisplayPlus
update (DisplayPlus size display nr changes) = DisplayPlus size display' nr' fchanges'
    where
    fchanges = reverse changes
    fchanges' = reverse changes'
    (display',_,nr',changes') = ud (0,0) [((0,0),size)] nr fchanges display
    ud :: Coord -> [Area] -> [Area] -> [DisplayChange] -> Display -> 
	    (Display,[Area],[Area],[DisplayChange])
    ud i_origin i_visible i_redraw i_changes im =
	let (im',redraw',changes') = ac i_redraw i_changes im
	    ac :: [Area] -> [DisplayChange] -> Display -> (Display,[Area],[DisplayChange])
	    ac redraw [] im = (im,redraw,[])
	    ac redraw changes@((cid,cf):cs) im@(Image imid _ _ _) =
		if cid == imid then
		    let (i,r) = cf im i_origin i_visible c_s_visible redraw
		    in ac r cs i
		else
		    (im,redraw,changes)
	
	    Image imid ii@(ImageInfo rpos _ vis _ _ _) children ov = im'

	    apos = addArea i_origin rpos
	    clipped = if vis then intersectArea apos i_visible else []
	    
	    (c_s_display,c_s_visible,c_s_redraw,c_s_changes) = 
		    if ov then 
			fov clipped redraw' changes' [] children 
		    else
			fdi clipped redraw' changes' [] children 
		where
		fdi i_visible3 i_redraw i_changes children' [] = 
		    (reverse children',i_visible3,i_redraw,i_changes)
		fdi i_visible2 i_redraw i_changes children' (c:cs) =
		    let (c_display,c_covers,c_redraw,c_changes) =
			    ud (fst apos) clipped i_redraw i_changes c
			i_visible2' = subtractAreas c_covers i_visible2
		    in fdi i_visible2' c_redraw c_changes (c_display:children') cs

		fov i_visible3 i_redraw i_changes children' [] = 
		    (reverse children',i_visible3,i_redraw,i_changes)
		fov i_visible2 i_redraw i_changes children' (c:cs) =
		    let (c_display,c_covers,c_redraw,c_changes) =
			    ud (fst apos) i_visible2 i_redraw i_changes c
			i_visible2' = subtractAreas c_covers i_visible2
		    in fov i_visible2' c_redraw c_changes (c_display:children') cs
	    
	    s_display = Image imid ii c_s_display ov

	    in
	    (s_display,clipped,c_s_redraw,c_s_changes)

mkImage :: ImageID -> Placement -> DrawFun -> Bool -> ClickAction -> ClickAction -> [Display] -> Bool -> Display
mkImage imid rpos@((x1,y1),(x2,y2)) df vis ca uca children ov =
    let size = (x2-x1,y2-y1)
    in Image imid (ImageInfo rpos df vis ca uca size) children ov

firstImage :: Size -> ImageID -> DrawFun -> Bool -> ClickAction -> ClickAction -> DisplayPlus
firstImage size imid df vis ca uca =
    DisplayPlus size (mkImage imid ((0,0),(0,0)) df vis ca uca [] True) [] []

cacheUpdate :: (ImageID,ChangeFun) -> DisplayPlus -> DisplayPlus
cacheUpdate u (DisplayPlus size display nr changes) = (DisplayPlus size display nr (u:changes))

cacheUpdates :: [(ImageID,ChangeFun)] -> DisplayPlus -> DisplayPlus
cacheUpdates us (DisplayPlus size display nr changes) = DisplayPlus size display nr (reverse us++changes)

addChild :: ImageID -> Display -> (ImageID,ChangeFun)
addChild imid child@(Image cid cii@(ImageInfo rpos _ cvis _ _ _) cc _) = (imid,ac)
    where
    ac :: ChangeFun
    ac (Image _ ii@(ImageInfo ppos _ vis _ _ _) children _) origin visible _ redraw =
	let porigin = fst (addArea origin ppos)
	    apos = addArea porigin rpos
	    a = intersectArea apos visible
	    redraw' = if vis && cvis then unionAreas a redraw else redraw
	in
	(Image imid ii (child:children) True,redraw')
		
addChildren :: ImageID -> [Display] -> (ImageID,ChangeFun)
addChildren imid newborns = (imid,ac)
    where
    ac :: ChangeFun
    ac (Image _ ii@(ImageInfo _ _ _ _ _ _) children _) origin visible _ redraw =
	-- *** ASSUMES THAT NEWBORNS ARE NOT VISIBLE ***
	(Image imid ii (newborns++children) True,redraw)
		
moveImage :: ImageID -> Coord -> (ImageID,ChangeFun)
moveImage imid nnw = (imid,mi)
    where
    mi :: ChangeFun
    mi (Image _ ii@(ImageInfo rpos df vis ca uca size) children ov)
	    origin visible _ redraw =
	let apos = addArea origin rpos
	    rpos' = (nnw,pairop (+) size nnw)
	    apos' = addArea origin rpos'
	    a = intersectArea apos' visible
	    b = intersectArea apos visible
	    redraw' = if vis && rpos /= rpos' then unionAreas (unionAreas a b) redraw else redraw
	in (Image imid (ImageInfo rpos' df vis ca uca size) children ov,redraw')
		
resizeImage :: ImageID -> Size -> (ImageID,ChangeFun)
resizeImage imid size' = (imid,ri)
    where
    ri :: ChangeFun
    ri (Image _ ii@(ImageInfo rpos@(rnw,rse) df vis ca uca size) children ov) 
	    origin visible _ redraw =
	let apos@(anw,ase) = addArea origin rpos
	    apos' = (anw,pairop (+) size' anw)
	    rpos' = (rnw,pairop (+) size' rnw)
	    a = intersectArea apos' visible
	    b = intersectArea apos visible
	    redraw' = if vis && size' /= size then unionAreas (unionAreas a b) redraw else redraw
	in (Image imid (ImageInfo rpos' df vis ca uca size') children ov,redraw')

newImageDrawFun :: ImageID -> DrawFun -> (ImageID,ChangeFun)
newImageDrawFun imid df' = (imid,nidf)
    where
    nidf :: ChangeFun
    nidf (Image _ ii@(ImageInfo rpos df vis ca uca size) children ov) 
	    origin _ cvisible redraw =
	let apos = addArea origin rpos
	    redraw' = if vis then unionAreas (intersectArea apos cvisible) redraw else redraw
	in (Image imid (ImageInfo rpos df' vis ca uca size) children ov,redraw')
		
quietImageDrawFun :: ImageID -> DrawFun -> (ImageID,ChangeFun)
quietImageDrawFun imid df' = (imid,nidf)
    where
    nidf :: ChangeFun
    nidf (Image _ ii@(ImageInfo rpos df vis ca uca size) children ov) 
	    _ _ _ redraw =
	 (Image imid (ImageInfo rpos df' vis ca uca size) children ov,redraw)
		
visibleImage :: ImageID -> (ImageID,ChangeFun)
visibleImage imid = (imid,vf)
    where
    vf :: ChangeFun
    vf (Image _ ii@(ImageInfo rpos df vis ca uca size) children ov) 
	    origin visible _ redraw =
	let apos = addArea origin rpos
	    ra = intersectArea apos visible
	    redraw' = if vis == False then 
		unionAreas (intersectArea apos visible) redraw else redraw
	in (Image imid (ImageInfo rpos df True ca uca size) children ov,redraw')
		
invisibleImage :: ImageID -> (ImageID,ChangeFun)
invisibleImage imid = (imid,ivf)
    where
    ivf :: ChangeFun
    ivf (Image _ ii@(ImageInfo rpos df vis ca uca size) children ov) 
	    origin visible _ redraw =
	let apos = addArea origin rpos
	    ra = intersectArea apos visible
	    redraw' = if vis == True then 
		unionAreas (intersectArea apos visible) redraw else redraw
	in (Image imid (ImageInfo rpos df False ca uca size) children ov,redraw')
		
imageClickAction :: ImageID -> ClickAction -> (ImageID,ChangeFun)
imageClickAction imid ca = (imid,caf)
    where
    caf :: ChangeFun
    caf (Image _ ii@(ImageInfo rpos df vis _ uca size) children ov) 
	    _ _ _ redraw =
	(Image imid (ImageInfo rpos df vis ca uca size) children ov,redraw)
		
imageUnClickAction :: ImageID -> ClickAction -> (ImageID,ChangeFun)
imageUnClickAction imid uca = (imid,ucaf)
    where
    ucaf :: ChangeFun
    ucaf (Image _ ii@(ImageInfo rpos df vis ca _ size) children ov) 
	    _ _ _ redraw =
	(Image imid (ImageInfo rpos df vis ca uca size) children ov,redraw)
		
