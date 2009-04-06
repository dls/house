module Area where
import GadgetsPrelude

-- ((x1,y1),(x2,y2)) = the rectangle with top-left (x1,y1) 
-- and bottom-right (x2-1,y2-1) inclusive.
{-
filterArea :: [Area] -> [Area]
filterArea [] = []
filterArea (c@((x1,y1),(x2,y2)):r) =
    if x1 >= x2 then filterArea r
    else if y1 >= y2 then filterArea r
    else if x1 >= maxXCoord || y1 >= maxYCoord || x2 < 0 || y2 < 0 then filterArea r
    else ((x1,y1),(x2,y2)):filterArea r
-}
addCoord :: Coord -> Coord -> Coord
addCoord (x1,y1) (x2,y2) = (x1+x2,y1+y2)

addArea :: Coord -> Area -> Area
addArea (x,y) ((x1,y1),(x2,y2)) = ((x1+x,y1+y),(x2+x,y2+y))

intersectArea :: Area -> [Area] -> [Area]
intersectArea ra as = concat (map (oc ra) as)
    -- given two areas, a and b, returns the area where 
    -- a intersects with b (or [] if no intersection)
    where
    oc :: Area -> Area -> [Area]
    oc a b =
	let ((ax1,ay1),(ax2,ay2)) = bltr a
	    ((bx1,by1),(bx2,by2)) = bltr b
	    yia x1 x2 =
		if x1 == x2 then
		    []
		else if ay1<=by1 then
		    if ay2<=by1 then
		        []
		    else
		        let my = min ay2 by2 in
			if by1 == my then
			    []
			else
			    [((x1,by1),(x2,my))]
		else if ay1>=by2 then
		    []
		else -- by1 < ay1 < by2
		    let my = min ay2 by2 in
		    if ay1 == my then
		        []
		    else
		        [((x1,ay1),(x2,my))] in
	if ax1<=bx1 then
	    if ax2<=bx1 then
		[]
	    else
		yia bx1 (min ax2 bx2)
	else if ax1>=bx2 then
	    []
	else -- bx1 < ax1 < bx2
	    yia ax1 (min ax2 bx2)

intersectAreas :: [Area] -> [Area] -> [Area]
intersectAreas [] bs = []
intersectAreas (a:as) bs = unionAreas (intersectArea a bs) (intersectAreas as bs)
 
subtractArea :: Area -> [Area] -> [Area]
subtractArea a as = concat (map (sa a) as)
    -- given two areas, a and b, returns a list of areas covering 
    -- the area (a subtracted from b), without any overlapping of areas.
    where
    sa :: Area -> Area -> [Area]
    sa a b =
	let ra@((ax1,ay1),(ax2,ay2)) = bltr a
	    rb@((bx1,by1),(bx2,by2)) = bltr b
            ysa l c r =
	        if ay1<=by1 then
	            if ay2<=by1 then
		        [rb]
	            else if ay2>=by2 then
		        col l by1 by2 ++ col r by1 by2
	            else -- by1 < ay2 < by2
		        col l by1 ay2 ++ col c ay2 by2 ++ col r by1 ay2
	        else if ay1>=by2 then
	            [rb]
	        else -- by1 < ay1 < by2
	            if ay2>=by2 then
	                col l ay1 by2 ++ col c by1 ay1 ++ col r ay1 by2
	            else -- by1 < ay2 < by2
		        col l ay1 ay2 ++ col c by1 ay1 ++ col c ay2 by2 ++ col r ay1 ay2 in
	if ay1 == ay2 || ax1 == ax2 then
	    [rb]
	else if ax1<=bx1 then
	    if ax2<=bx1 then
		[rb]
	    else if ax2>=bx2 then
		ysa None (Yes (bx1,bx2)) None
	    else -- bx1 < ax2 < bx2
		ysa None (Yes (bx1,bx2)) (Yes (ax2,bx2))
	else if ax1>=bx2 then
	    [rb]
	else -- bx1 < ax1 < bx2
	    if ax2>=bx2 then
		ysa (Yes (bx1,ax1)) (Yes (bx1,bx2)) None
	    else -- bx1 < ax2 < bx2
		ysa (Yes (bx1,ax1)) (Yes (bx1,bx2)) (Yes (ax2,bx2))
    col None _ _ = []
    col (Yes (x1,x2)) y1 y2 = [((x1,y1),(x2,y2))]

subtractAreas :: [Area] -> [Area] -> [Area]
subtractAreas [] bs = bs
subtractAreas (a:as) bs = subtractAreas as (subtractArea a bs)

-- unionArea:
-- given an area 'ra' and a list of (non-overlapping) areas 'as', returns a
-- new list of (non-overlapping) areas that cover the union of area 'ra'
-- and areas 'as'.
-- It does this by:
--   1) subtracting each area of 'as' from 'ra' (incrementally)
--      (which leaves only rectangles within 'ra' that do not overlap
--       with any rectangles in 'as')
--   2) appending 'as' to the reduced 'ra'
unionArea :: Area -> [Area] -> [Area]
unionArea ra [] = [ra]
unionArea ra as = red [ra] as ++ as
    where
    red aa [] = aa
    red aa (n:ns) = red (subtractArea n aa) ns

unionAreas :: [Area] -> [Area] -> [Area]
unionAreas [] bs = bs
unionAreas (a:as) bs = unionAreas as (unionArea a bs)

-- bltr: turns area coords into bottom left, top right form.
bltr :: Area -> Area
bltr ((ax1,ay1),(ax2,ay2)) = ((min ax1 ax2,min ay1 ay2),(max ax1 ax2,max ay1 ay2))
