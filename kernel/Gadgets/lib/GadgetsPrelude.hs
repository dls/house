-- File was called gadgets.gs
module GadgetsPrelude(module GadgetsPrelude,module GUIPrelude) where
import Prelude hiding (Maybe(..))
import GUIPrelude
import Text.Show.Functions
default(Int)

-- Various types of component

--lall = ":a combination.gs explode.gs updown.gs escher.gs bristol.gs"

type SystemWires = InOut OSResponse OSRequest
type GadgetWires = (Out SMRequest,InOut LOResponse LORequest,InOut SMResponse SMRequest)
type WindowWires = Out Coord
type DefaultValues = Int

data ComponentState = ComponentState DefaultValues
data SystemState = SystemState SystemWires DefaultValues
data GadgetState = GadgetState GadgetWires SystemWires DefaultValues
data WindowState = WindowState WindowWires GadgetWires SystemWires DefaultValues

type Component = Process ComponentState
type System = Process SystemState
type Gadget = Process GadgetState
type Window = Process WindowState

initGap = 20

type Action a s = (a -> Process s) -> Process s

type Procedure s = Process s -> Process s
type Function a s = (a -> Process s) -> Process s

-- Some type stuff

type Change a = a -> a

data Maybe a = Yes a | None deriving (Eq,Show)

none None = True
none _ = False

isYes :: Maybe a -> Bool
isYes (Yes _) = True
isYes _ = False

data SMRequest
    = SMNewCon (In SMRequest,Out SMResponse)
    | SMMoveTo Coord
    | SMMySize Size
    | SMImageSize Size
    | SMImagePos Coord
    | SMAlterImagePos (Coord->Coord)
    | SMDrawFun DrawFun
    | SMUpdateDrawFun DrawFun DrawFun
    | SMDumpInfo
    | SMInvisible
    | SMVisible
    | SMRedraw
    | SMDoAction (Component -> Component)
    | SMClickAction ClickAction
    | SMUnClickAction ClickAction
    | SMMulti [SMRequest]
    | SMFor (Out SMResponse) SMRequest
    | SMClaimFocus
    | SMDisownFocus
    | SMUpdates [DisplayChange]
    | SMAddChild Display [(In SMRequest,Out SMResponse)]
data SMResponse
    = SMMouseClick Int Int Int
    | SMMouseUnClick Int Int Int
    | SMKeyPress Char
    | SMLoseFocus
 
data OSRequest
    = OSNewCon (In OSRequest,Out OSResponse)
    | OSPassToSM SMRequest
data OSResponse
    = OSResponse

data LORequest
    = LOSize Size (Coord -> [DisplayChange]) (Coord -> [DisplayChange])
    | LOInit Size (Coord -> [DisplayChange]) (Coord -> [Display]) [(InOut SMRequest SMResponse)]
data LOResponse
    = LOMoveTo Coord

-- ****** Useful things

colour c = DrawSetColour (col c)

col "white" = (65535,65535,65535)
col "paper" = (50000,50000,50000)
col "light grey" = (43767,43767,43767)
col "very light grey" = (55000,55000,55000)
col "grey" = (33767,33767,33767)
col "dark grey" = (23767,23767,23767)
col "black" = (0,0,0)
col "red" = (65535,0,0)
col "orange" = (65535,42240,0)
col "green" = (0,65535,0)
col "blue" = (0,0,65535)
col "cyan" = (0,65535,65535)
col "yellow" = (65535,65535,0)
col "grey yellow" = (32767,32767,0)
col "light yellow" = (65535,65535,32767)

mixCol :: Colour -> Colour -> Colour
mixCol (r1,g1,b1) (r2,g2,b2) = (a r1 r2,a g1 g2,a b1 b2)
    where
    a v1 v2 = (v1+v2) `div` 2

-- fg bg flat shadow light highlight focus enabled disabled one two three four five

colourset = 
    if False then
	(col "black",col "white", col "white",col "black", col "black",
	col "white", col "white",col "black",col "white",col "black",col "white",
	col "black",col "white",col "black")
    else
	(col "black",col "paper", col "light grey",col "dark grey", col "white",
	col "yellow", col "light yellow",col "grey yellow",col "light grey",col "red",col "green",
	col "blue",col "cyan",col "orange")

-- ****** Useful functions

moveOrigin :: (Int,Int) -> [ScreenCmnd] -> [ScreenCmnd]
moveOrigin (x,y) s = map (mo x y) s
    where
    mo x y (DrawSetPixel (x1,y1) c) = DrawSetPixel (x1+x,y1+y) c
    mo x y (DrawLine ((x1,y1),(x2,y2))) = DrawLine ((x1+x,y1+y),(x2+x,y2+y))
    mo x y (DrawFilledRectangle ((x1,y1),(x2,y2))) = DrawFilledRectangle ((x1+x,y1+y),(x2+x,y2+y))
    mo x y (DrawPixmap pm (x1,y1)) = DrawPixmap pm (x1+x,y1+y)
    mo x y (DrawText (x1,y1) s) = DrawText (x1+x,y1+y) s
    mo x y (DrawFilledTriangle ((x1,y1),(x2,y2),(x3,y3))) = 
	DrawFilledTriangle ((x1+x,y1+y),(x2+x,y2+y),(x3+x,y3+y))
    mo x y (DrawCircle ((x1,y1),r)) = DrawCircle ((x1+x,y1+y),r)
    mo x y (DrawFilledCircle ((x1,y1),r)) = DrawFilledCircle ((x1+x,y1+y),r)
    mo x y c = c
 
scale :: (Float,Float) -> [ScreenCmnd] -> [ScreenCmnd]
scale (x,y) = map sc
    where
    sc (DrawSetPixel (x1,y1) c) = DrawSetPixel (sx x1,sy y1) c -- hmm
    sc (DrawLine ((x1,y1),(x2,y2))) = DrawLine ((sx x1,sy y1),(sx x2,sy y2))
    sc (DrawFilledRectangle ((x1,y1),(x2,y2))) = DrawFilledRectangle ((sx x1,sy y1),(sx x2,sy y2))
    sc (DrawFilledTriangle ((x1,y1),(x2,y2),(x3,y3))) = 
	DrawFilledTriangle ((sx x1,sy y1),(sx x2,sy y2),(sx x3,sy y3))
    sc (DrawCircle _) = error "Can't scale circle"
    sc (DrawFilledCircle _) = error "Can't scale filled circle"
    sc (DrawText _ s) = error ("Can't scale text '"++s++"'")
    sc c = c
    sx n = truncate ((fromIntegral n)*x)
    sy n = truncate ((fromIntegral n)*y)

turnCW :: DrawFun -> DrawFun
turnCW df (x,y) (a,b) = map t (df (y,x) (tc a,tc b))
    where
    t :: ScreenCmnd -> ScreenCmnd
    t (DrawSetPixel p c) = DrawSetPixel (tc p) c
    t (DrawLine (a,b)) = (DrawLine (tc a,tc b))
    t (DrawFilledRectangle (a,b)) = (DrawFilledRectangle (tc a,tc b))
    t (DrawPixmap pm a) = DrawPixmap pm (tc a) -- hmm
    t (DrawFilledTriangle (a,b,c)) = (DrawFilledTriangle (tc a,tc b,tc c))
    t (DrawFilledCircle ((a,b),c)) = (DrawFilledCircle ((a,b),c))
    t (DrawCircle ((a,b),c)) = (DrawCircle ((a,b),c))
    t (DrawText _ s) = error ("Can't turnCW text '"++s++"'")
    t c = c
    tc (a,b) = (x-1-b,a)

dfplus d1 d2 s r = d1 s r ++ d2 s r

dfToSc (sx,sy) df = df (sx,sy) ((0,0),(sx,sy))

scToDf (sx,sy) sc (x,y) _ = scale (fx,fy) sc
    where
    fx = (fromIntegral x)/(fromIntegral sx)
    fy = (fromIntegral y)/(fromIntegral sy)

drawbox ((a,b),(c,d)) = [drawline ((a,b),(a,d)),drawline ((a,d),(c,d)),
    drawline ((c,d),(c,b)),drawline ((c,b),(a,b))]

fillbox = DrawFilledRectangle
fillrect ((x1,y1),(x2,y2)) = 
    let x3 = x2 - 1
	y3 = y2 - 1 in
    if x1 /= x3 && y1 /= y3 then [DrawFilledRectangle ((x1,y1),(x3,y3))] else []
drawline = DrawLine
blank (x,y) _ = [colour "white",fillbox ((0,0),(x-1,y-1))]
blackbg (x,y) _ = [colour "black",fillbox ((0,0),(x-1,y-1))]
bonwbox (x,y) _ = 
    [colour "white",
    fillbox ((0,0),(x-1,y-1)),
    colour "black"]++drawbox ((0,0),(x-1,y-1))
colourbox c (x,y) _ = [DrawSetColour c,fillbox ((0,0),(x-1,y-1))]
transparent _ _ = []
plinth g f b i (xo,yo) (x,y) = case g of
    0 -> []
    1 -> [DrawSetColour f,drawline ((xo+0,0+yo),(xo+x,0+yo)),drawline ((xo+0,0+yo),(xo+0,y+yo)),
	    DrawSetColour b,drawline ((xo+x,y+yo),(xo+0,y+yo)),drawline ((xo+x,y+yo),(xo+x,0+yo))]
    2 -> [DrawSetColour f,drawline ((xo+0,0+yo),(xo+x,0+yo)),drawline ((xo+1,1+yo),(xo+x-1,1+yo)),
	    drawline ((xo+0,1+yo),(xo+0,y+yo)),drawline ((xo+1,2+yo),(xo+1,y-1+yo)),
	    DrawSetColour b,drawline ((xo+1,y+yo),(xo+x,y+yo)),
	    drawline ((xo+2,y-1+yo),(xo+x-1,y-1+yo)),
	    drawline ((xo+x,y-1+yo),(xo+x,1+yo)),drawline ((xo+x-1,y-2+yo),(xo+x-1,2+yo))]
    otherwise -> [DrawSetColour f,drawline ((xo+0,0+yo),(xo+x,0+yo)),
	    drawline ((xo+0,1+yo),(xo+0,y+yo)),
	    drawline ((xo+g,y-g+1+yo),(xo+x-g+1,y-g+1+yo)),
	    drawline ((xo+x-g+1,y-g+yo),(xo+x-g+1,g-1+yo)),
	    DrawSetColour b,drawline ((xo+1,y+yo),(xo+x,y+yo)),
	    drawline ((xo+x,y-1+yo),(xo+x,1+yo)),
	    drawline ((xo+g-1,g-1+yo),(xo+x-g,g-1+yo)),
	    drawline ((xo+g-1,g+yo),(xo+g-1,y-g+yo+1)),
	    DrawSetColour i,fillbox ((xo+1,1+yo),(xo+g-2,y-1+yo)),
	    fillbox ((xo+x-g+2,1+yo),(xo+x-1,y-1+yo)),
	    fillbox ((xo+g-1,1+yo),(xo+x-g+1,g-2+yo)),
	    fillbox ((xo+g-1,y-g+2+yo),(xo+x-g+1,y-1+yo))]

ctext f (x,y) s = [DrawSetFont f, DrawText ((x-xs)`div`2,(y+a)`div`2) s]
    where
    (xs,(a,_)) = textSize f s

text f c s (x,y) _ = [DrawSetColour c, DrawSetFont f, DrawText ((x-xs)`div`2,(y+a)`div`2) s]
    where
    (xs,(a,_)) = textSize f s

--font f = DrawSetFont f

textat f (x,y) s = [DrawSetFont f, DrawText (x-(xs `div` 2),y+(ys `div` 2)) s]
	where
	(xs,(ys,_)) = textSize f s

rxFail n = error ("No corresponding 'from' guard for message received in "++n)

split :: (a->Bool) -> [a] -> ([a],[a])
split p [] = ([],[])
split p xs@(h:t) =
    if p h then
        let (ys,ns) = split p t in (h:ys,ns)
    else
        let (ys,ns) = split p t in (ys,h:ns)
 
sortby :: Ord a => (b->a) -> [b] -> [b]
sortby e = foldr (insertby e) []
 
insertby :: Ord a => (b->a) -> b -> [b] -> [b]
insertby e x [] = [x]
insertby e x (y:ys)
        | e x <= e y = x:y:ys
        | otherwise = y:insertby e x ys

both :: (a->b->Bool) -> (a,a) -> (b,b) -> Bool
both f (a,b) (c,d) = a `f` c && b `f` d

pairop :: (a->b->c) -> (a,a) -> (b,b) -> (c,c)
pairop op (a,b) (c,d) = (a `op` c,b `op` d)

-- ****** Useful continuations

debug :: String -> Process a -> Process a
--debug = typeout
debug _ c = c

dump d g c = 
    typeout ("*** "++d++" ***\n") $
    showGraph g $
    typeout "\n***\n" $
    c

setState :: a -> Process a -> Process a
setState ns (P c) = P $ \ _ -> c ns

readState :: (a -> Process a) -> Process a
readState c = P $ \ s -> unP (c s) s

updateState f (P c) = P $ c . f

-- ****** Constants (oh for #define...!)

--maxXCoord = 1142
--maxYCoord = 868

-- TYPES

type Size = (Int,Int)
type Position = (Int,Int)
type Placement = (Position,Position)
type Coord = (Int,Int)
type Area = (Coord,Coord)
-- DrawFun = (size of image) -> (area that needs redrawing) -> Commands
type DrawFun = Size -> Area -> [ScreenCmnd]
type Visibility = Bool
type ImageID = Out SMResponse
type ClickAction = (Coord -> Int -> Component -> Component)
data ImageInfo
    = ImageInfo Placement DrawFun Visibility ClickAction ClickAction Size 
   -- {                    specified                               } calculated
type Colour = (Int,Int,Int)

data Display = Image ImageID ImageInfo [Display] Bool

data DisplayPlus = DisplayPlus Size Display [Area] [DisplayChange]

type DisplayChange = (ImageID,ChangeFun)
type ChangeFun = Display -> Coord -> [Area] -> [Area] -> [Area] -> (Display,[Area])

--instance Show (In a) where showsPrec _ (In n) = showString ("i"{-++show n-})
--instance Show (Out a) where showsPrec _ (Out n) = showString ("o"{-++show n-})

-- instance Eq (Out a) where (Out n) == (Out m) = n == m
{-
instance Ord (Out a) where
    (Out n) < (Out m) = n<m
    (Out n) > (Out m) = n>m
    (Out n) <= (Out m) = n <= m
    (Out n) >= (Out m) = n >= m
-}
instance Show DisplayPlus where
  showsPrec _ (DisplayPlus s d nr c) =
    showString ("DisplayPlus "++show s++"\n-"++show d++"-"++show nr++"\n-"++show c++"\n")

--instance Show DisplayChange where
--    showsPrec _ (n,_) = showString (show n)

instance Show Display where
    showsPrec _ w = showString (showWin 0 w)
        where
        showWin n (Image aid ai [] _) = space n++"Image "++show aid++" "++show ai++" []\n"
        showWin n (Image aid ai chi True) = space n++"Image "++show aid++" "++show ai++" overlapping ["++showChi (n+2) chi++space n++"]\n"
        showWin n (Image aid ai chi False) = space n++"Image "++show aid++" "++show ai++" non-overlapping ["++showChi (n+2) chi++space n++"]\n"
	showChi n [] = ""
	showChi n (c:cs) = "\n"++showWin n c++showChi n cs

instance Show ImageInfo where
    showsPrec _ (ImageInfo rpl _ v _ _ s) = 
	showString ("(AI "++show rpl++" "++show v++" "++show s++")")

{-
instance Show ScreenCmnd where
    showsPrec _ (DrawSetClip c) = showString ("DrawSetClip "++show c)
    showsPrec _ (DrawSetColour c) = showString ("DrawSetColour "++show c)
    showsPrec _ (DrawLine l) = showString ("DrawLine "++show l)
    showsPrec _ (DrawFilledRectangle r) = showString ("DrawFilledRectangle "++show r)
-}
{-
unzip3 :: [(a,b,c)] -> ([a],[b],[c])
unzip3 = foldr (\(a,b,c) ~(as,bs,cs) -> (a:as, b:bs, c:cs)) ([], [], [])

unzip4 :: [(a,b,c,d)] -> ([a],[b],[c],[d])
unzip4 = foldr (\(a,b,c,d) ~(as,bs,cs,ds) -> (a:as, b:bs, c:cs, d:ds)) ([], [], [], [])
-}
space n = replicate n ' '
