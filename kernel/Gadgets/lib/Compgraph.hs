module Compgraph where

man = Above [AlignRight [face,small],body,legs]
    where
    face = Circle 49
    small = Circle 20
    body = FilledTriangle ((40,100),(-40,100))
    legs = Beside [leg,leg]
    leg = FilledTriangle ((20,-100),(-20,-100))

defaultStyle = (col "black","")

font f (c,_) = ((c,f),DrawSetFont f)
pen c (_,f) = ((c,f),DrawSetColour c)

type FontName = String
type PictureStyle = (Colour,FontName)
type Vector = (Int,Int)

data Picture i
    = Space Size
    | Point
    | Line Vector
    | Rectangle (Vector,Vector)
    | FilledRectangle (Vector,Vector)
    | Triangle (Vector,Vector)
    | FilledTriangle (Vector,Vector)
    | Circle Int
    | FilledCircle Int
    | Text String
    | Beside [Picture i]
    | AlignTop [Picture i]
    | AlignBottom [Picture i]
    | Above [Picture i]
    | AlignLeft [Picture i]
    | AlignRight [Picture i]
    | Group (Picture i)
    | Layered [Picture i]
    | Label i (Picture i)
    | Style (PictureStyle -> (PictureStyle,ScreenCmnd)) (Picture i)

-- sizeP needs altering to take into account line thickness
sizeP :: Picture i -> Size
sizeP = sizeP' defaultStyle 
    where
    sizeP' :: PictureStyle -> Picture i -> Size
    sizeP' _ (Space s) = s
    sizeP' _ (Point) = (0,0)
    sizeP' _ (Line s) = s
    sizeP' _ (Rectangle ((x1,y1),(x2,y2))) = (maxx-minx,maxy-miny)
	where 
	maxx = max (max x1 x2) 0
	minx = min (min x1 x2) 0
	maxy = max (max y1 y2) 0
	miny = min (min y1 y2) 0
    sizeP' _ (FilledRectangle ((x1,y1),(x2,y2))) = (maxx-minx,maxy-miny)
	where 
	maxx = max (max x1 x2) 0
	minx = min (min x1 x2) 0
	maxy = max (max y1 y2) 0
	miny = min (min y1 y2) 0
    sizeP' _ (Triangle ((x1,y1),(x2,y2))) = (maxx-minx,maxy-miny)
	where 
	maxx = max (max x1 x2) 0
	minx = min (min x1 x2) 0
	maxy = max (max y1 y2) 0
	miny = min (min y1 y2) 0
    sizeP' _ (FilledTriangle ((x1,y1),(x2,y2))) = (maxx-minx,maxy-miny)
	where 
	maxx = max (max x1 x2) 0
	minx = min (min x1 x2) 0
	maxy = max (max y1 y2) 0
	miny = min (min y1 y2) 0
    sizeP' _ (Circle r) = let rr = r+r in (rr,rr)
    sizeP' _ (FilledCircle r) = let rr = r+r in (rr,rr)
    sizeP' (_,fn) (Text s) = (x,a+d)
	where (x,(a,d)) = textSize fn s
    sizeP' sty (Beside ps) = (sum xs, foldl max 0 ys)
	where (xs,ys) = unzip (map (sizeP' sty) ps)
    sizeP' sty (AlignTop ps) = (sum xs, foldl max 0 ys)
	where (xs,ys) = unzip (map (sizeP' sty) ps)
    sizeP' sty (AlignBottom ps) = (sum xs, foldl max 0 ys)
	where (xs,ys) = unzip (map (sizeP' sty) ps)
    sizeP' sty (Above ps) = (foldl max 0 xs, sum ys)
	where (xs,ys) = unzip (map (sizeP' sty) ps)
    sizeP' sty (AlignLeft ps) = (foldl max 0 xs, sum ys)
	where (xs,ys) = unzip (map (sizeP' sty) ps)
    sizeP' sty (AlignRight ps) = (foldl max 0 xs, sum ys)
	where (xs,ys) = unzip (map (sizeP' sty) ps)
    sizeP' sty (Group p) = sizeP' sty p
    sizeP' sty (Layered ps) = (foldl max 0 xs, foldl max 0 ys)
	where (xs,ys) = unzip (map (sizeP' sty) ps)
    sizeP' sty (Label _ p) = sizeP' sty p
    sizeP' sty (Style c p) = sizeP' sty' p
	where
	(sty',_) = c sty
    sizeP' _ _ = error "don't know how to size that"

plotP :: Coord -> [ScreenCmnd] -> Picture i -> [ScreenCmnd]
plotP = plotP' defaultStyle
    where
    plotP' :: PictureStyle -> Coord -> [ScreenCmnd] -> Picture i -> [ScreenCmnd]
    plotP' _ _ sc (Space s) = sc
    plotP' _ p sc (Point) = ((DrawLine (p,p)):sc)	--!!! could do with DrawPoint
    plotP' _ (x1,y1) sc (Line (dx,dy)) = ((DrawLine ((x1,y1),(x1+dx,y1+dy))):sc)
    plotP' _ (x1,y1) sc (Rectangle ((dx1,dy1),(dx2,dy2))) = 
	let b = (x1+dx1-ox,y1+dy1-oy)
	    c = (x1+dx2-ox,y1+dy2-oy)
	    d = (x1+dx1+dx2-ox,y1+dy1+dy2-oy) 
	    ox = min (min dx1 dx2) 0
	    oy = min (min dy1 dy2) 0
	    x1' = x1-ox
	    y1' = y1-oy
	in
	(DrawLine ((x1',y1'),b)):(DrawLine ((x1',y1'),c)):(DrawLine (c,d)):(DrawLine (b,d)):sc
    plotP' _ (x1,y1) sc (FilledRectangle ((dx1,dy1),(dx2,dy2))) = 
	let b = (x1+dx1-ox,y1+dy1-oy)
	    c = (x1+dx2-ox,y1+dy2-oy)
	    d = (x1+dx1+dx2-ox,y1+dy1+dy2-oy) 
	    ox = min (min dx1 dx2) 0
	    oy = min (min dy1 dy2) 0
	    x1' = x1-ox
	    y1' = y1-oy
	in
	(DrawFilledTriangle ((x1',y1'),b,c)):(DrawFilledTriangle (b,c,d)):sc
    plotP' _ (x1,y1) sc (Triangle ((dx1,dy1),(dx2,dy2))) = 
	let b = (x1+dx1-ox,y1+dy1-oy)
	    c = (x1+dx2-ox,y1+dy2-oy) 
	    ox = min (min dx1 dx2) 0
	    oy = min (min dy1 dy2) 0
	    x1' = x1-ox
	    y1' = y1-oy
	in
	(DrawLine ((x1',y1'),b)):(DrawLine ((x1',y1'),c)):(DrawLine (c,b)):sc
    plotP' _ (x1,y1) sc (FilledTriangle ((dx1,dy1),(dx2,dy2))) = 
	let b = (x1+dx1-ox,y1+dy1-oy)
	    c = (x1+dx2-ox,y1+dy2-oy) 
	    ox = min (min dx1 dx2) 0
	    oy = min (min dy1 dy2) 0
	in
	((DrawFilledTriangle ((x1-ox,y1-oy),b,c)):sc)
    plotP' _ (x1,y1) sc (Circle r) = (DrawCircle ((x1+r,y1+r),r)):sc
    plotP' _ (x1,y1) sc (FilledCircle r) = (DrawFilledCircle ((x1+r,y1+r),r)):sc
    plotP' sty (x1,y1) sc (Beside ps) = f x1 xs ys ps sc
	where
	(xs,ys) = unzip (map sizeP ps)
	my = foldr max 0 ys
	f _ [] [] [] sc = sc
	f px (x:xs) (y:ys) (p:ps) sc = f (px+x) xs ys ps sc'
	    where
	    sc' = plotP' sty (px,y1+((my-y) `div` 2)) sc p
    plotP' sty (x1,y1) sc (AlignTop ps) = f x1 xs ps sc
	where
	(xs,_) = unzip (map sizeP ps)
	f _ [] [] sc = sc
	f px (x:xs) (p:ps) sc = f (px+x) xs ps sc'
	    where
	    sc' = plotP' sty (px,y1) sc p
    plotP' sty (x1,y1) sc (AlignBottom ps) = f x1 xs ys ps sc
	where
	(xs,ys) = unzip (map sizeP ps)
	my = foldr max 0 ys
	f _ [] [] [] sc = sc
	f px (x:xs) (y:ys) (p:ps) sc = f (px+x) xs ys ps sc'
	    where
	    sc' = plotP' sty (px,y1+my-y) sc p
    plotP' sty (x1,y1) sc (Above ps) = f y1 xs ys ps sc
	where
	(xs,ys) = unzip (map sizeP ps)
	mx = foldr max 0 xs
	f _ [] [] [] sc = sc
	f py (x:xs) (y:ys) (p:ps) sc = f (py+y) xs ys ps sc'
	    where
	    sc' = plotP' sty (y1+((mx-x) `div` 2),py) sc p
    plotP' sty (x1,y1) sc (AlignLeft ps) = f y1 ys ps sc
	where
	(_,ys) = unzip (map sizeP ps)
	f _ [] [] sc = sc
	f py (y:ys) (p:ps) sc = f (py+y) ys ps sc'
	    where
	    sc' = plotP' sty (y1,py) sc p
    plotP' sty (x1,y1) sc (AlignRight ps) = f y1 xs ys ps sc
	where
	(xs,ys) = unzip (map sizeP ps)
	mx = foldr max 0 xs
	f _ [] [] [] sc = sc
	f py (x:xs) (y:ys) (p:ps) sc = f (py+y) xs ys ps sc'
	    where
	    sc' = plotP' sty (y1+mx-x,py) sc p
    plotP' sty po sc (Group p) = plotP' sty po sc p
    plotP' _ _ sc (Layered []) = sc
    plotP' sty po sc (Layered (p:ps)) = plotP' sty po sc' (Layered ps)
	where
	sc' = plotP' sty po sc p
    plotP' sty po sc (Label _ p) = plotP' sty po sc p
    plotP' sty po sc (Style c p) = csc:plotP' sty' po sc p
	where
	(sty',csc) = c sty
    plotP' _ _ _ _ = error "don't know how to plot that"

drawPicture :: Picture i -> DrawFun
drawPicture p (x,y) _ = (colour "black"):plotP (0,0) [] p
