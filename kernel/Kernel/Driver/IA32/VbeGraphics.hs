module Kernel.Driver.IA32.VbeGraphics(
   Graphics(..),VbeInfo(..),PixelValue,initialize,--old_initialize,
   drawLines,drawCircle,drawText,drawTextBox)
  where
import Control.Monad
import Util.Font
import Data.Array.IArray((!))
import Data.Array.Unboxed(UArray,listArray)
import Data.List(nub)
import H.Monad(H,liftIO)
import H.Concurrency
import H.Mutable
import qualified H.AdHocMem as U(Ptr)
import H.MemRegion(MemRegion,Offset,Storable,createRegion,--withTempRegion,
		   pokeElemOff,peekElemOff,fillRegion,moveBytes,pokeArray)
import Kernel.Bits
import Foreign.C(CString,peekCString)
import Util.Word24

default(Int,Double)

data Primary = Red | Green | Blue deriving (Eq,Bounded,Enum)

type ColorMap = UArray Word8 PixelValue
type PixelArray = UArray Int Word8

data Graphics
  = Graphics {
       screenSize    :: (Int,Int),
       fbInfo        :: FrameBufferInfo,
       vbeInfo       :: H VbeInfo,
       pixelValue    :: (Int,Int,Int) -> PixelValue,
       setColor      :: (Int,Int,Int) -> H (),
       setPixelValue :: PixelValue -> H (),
       setPixel      :: (Int,Int) -> H (),
       drawLine      :: (Int,Int) -> (Int,Int) -> H (),
       fillRectangle :: Int->Int->Int->Int -> H (),
       drawPixmap    :: (Int,Int) -> (Int,Int) -> ColorMap -> Maybe Word8 -> PixelArray ->H (),
       setClipRect   :: Int -> Int -> Int -> Int -> H (),
       moveCursor    :: Int -> Int -> H (),
       hideCursor, showCursor :: H ()
     }

data FrameBufferInfo
   = FBInfo {
       bpp         :: Int, -- bytes per pixel
       frameBuffer :: MemRegion,
       redMaskSize,greenMaskSize,blueMaskSize,
       redFieldPosition,greenFieldPosition,blueFieldPosition::Int
     }
   deriving (Show)

data VbeInfo
  = VbeInfo {
     version :: Int,
     oemString,oemVendorName,oemProductName :: String
    }
  deriving (Show)
{-
old_initialize =
  do size@(w,h) <- gfx_screen_size
     fbinfo <- frameBufferInfo
     if w==0 || h==0
	then return Nothing
	else do show_cursor
                return $ Just $ Graphics {
		     screenSize = size,
		     vbeInfo = getVbeInfo,
		     fbInfo = fbinfo,
		     setColor = setColor,
		     setPixel = uncurry set_pixel,
		     drawLine = drawLine,
		     fillRectangle = fill_rectangle,
		     setClipRect = set_clip,
		     moveCursor = set_cursor,
		     hideCursor = hideCursor
	           }
  where
    setColor (r,g,b) = set_color r g b
    drawLine (x1,y1) (x2,y2) = draw_line x1 y1 x2 y2
-}

withHiddenCursor gfx m = do hideCursor gfx; x<-m; showCursor gfx;return x

getVbeInfo = liftM4 VbeInfo vbeVersion oemString oemVendorName oemProductName
  where
    oemString = liftIO . peekCString =<< vbeOemstring
    oemVendorName = liftIO . peekCString =<< vbeOemvendorname
    oemProductName = liftIO . peekCString =<< vbeOemproductname

gfxScreenSize = liftM2 (,) gfxWidth gfxHeight

frameBufferInfo :: (Int,Int) -> H FrameBufferInfo
frameBufferInfo (w,h) =
  do bpp <- gfxBpp
     fb  <- gfxFramebuffer
     let fbsize = toEnum (w*h*bpp)
               -- ^ Should use the totalmemory field from the control info bock
	       -- for the actual size of the frame buffer...
         colors = [Red,Green,Blue]
     [rm,gm,bm] <- mapM (gfxMaskSize.fromEnum) colors
     [rp,gp,bp] <- mapM (gfxFieldPosition.fromEnum) colors
     return $ FBInfo { bpp=bpp,
		       frameBuffer=createRegion fb fbsize,
		       redMaskSize=rm,
		       greenMaskSize=gm,
		       blueMaskSize=bm,
		       redFieldPosition=rp,
		       greenFieldPosition=gp,
		       blueFieldPosition=bp }

-- Haskell versions for speed comparisons:

--fillRectangle x0 y0 w h =
--  hideCursor $ sequence_ [set_pixel x y|x<-[x0..x0+w-1],y<-[y0..y0+h-1]]

{-
fillRectangle x0 y0 w 0 = x0 `seq` y0 `seq` return ()
fillRectangle x0 y0 w n = fillLine x0 y0 w >> fillRectangle x0 (y0+1) w (n-1)
  where
    fillLine x y 0 = x `seq` y `seq` return ()
    fillLine x y n = set_pixel x y >> fillLine (x+1) y (n-1)
-}

---

drawLines gfx [] = return ()
drawLines gfx (p:ps) = withHiddenCursor gfx $ drawLines' p ps
  where
   drawLines' p1 [] = setPixel gfx p
   drawLines' p1 (p2:ps) = drawLine gfx p1 p2 >> drawLines' p2 ps

drawCircle gfx x y r =
    drawLines gfx [(round (rx+rr*cos a),round (ry+rr*sin a))|
	           n<-[0..2*r],
	           let a = da*fromIntegral n]
  where
    da = pi/rr
    rx = fromIntegral x
    ry = fromIntegral y
    rr = fromIntegral r

drawText gfx Fixed{width=w,ascent=a,shape=sh} x y s =
    withHiddenCursor gfx $ zipWithM_ drawChar [0..] s
  where
    t = y-a
    drawChar n c = sequence_ [setPixel gfx (cx+x,t+y)|(x,y)<-sh!c]
      where cx=x+w*n

drawTextBox gfx font bg fg x y s =
  withHiddenCursor gfx $
  do let (w,(a,d)) = textSize font s
     setColor gfx bg
     fillRectangle gfx x (y-a) w (a+d)
     setColor gfx fg
     drawText gfx font x y s

{-
foreign import ccall "gfx.h" set_color      :: Int->Int->Int->IO ()
foreign import ccall "gfx.h" set_pixel      :: Int->Int->IO ()
foreign import ccall "gfx.h" draw_line      :: Int->Int->Int->Int->IO ()
foreign import ccall "gfx.h" fill_rectangle :: Int->Int->Int->Int->IO ()
foreign import ccall "gfx.h" set_clip       :: Int->Int->Int->Int->IO ()
foreign import ccall "gfx.h" set_cursor     :: Int->Int->IO ()

foreign import ccall "gfx.h" hide_cursor    :: IO ()
foreign import ccall "gfx.h" show_cursor    :: IO ()
-}

foreign import ccall "gfx.h" gfx_width          :: IO Int
foreign import ccall "gfx.h" gfx_height         :: IO Int
foreign import ccall "gfx.h" gfx_bpp            :: IO Int -- bytes per pixel
foreign import ccall "gfx.h" gfx_mask_size      :: Int -> IO Int
foreign import ccall "gfx.h" gfx_field_position :: Int -> IO Int
foreign import ccall "gfx.h" gfx_framebuffer    :: IO (U.Ptr Word8)

gfxWidth = liftIO gfx_width
gfxHeight = liftIO gfx_height
gfxBpp = liftIO gfx_bpp
gfxMaskSize = liftIO . gfx_mask_size
gfxFieldPosition = liftIO . gfx_field_position
gfxFramebuffer = liftIO gfx_framebuffer

-- Reading the VBE control info block
foreign import ccall "gfx.h" vbe_version        :: IO Int
foreign import ccall "gfx.h" vbe_oemstring      :: IO CString
foreign import ccall "gfx.h" vbe_oemvendorname  :: IO CString
foreign import ccall "gfx.h" vbe_oemproductname :: IO CString

vbeVersion = liftIO vbe_version
vbeOemstring = liftIO vbe_oemstring
vbeOemvendorname = liftIO vbe_oemvendorname
vbeOemproductname = liftIO vbe_oemproductname

--------------------------------------------------------------------------------

type Clip = ((Int,Int),(Int,Int))
type PixelValue = Word32

-- Implementing all graphics primitives in Haskell!
initialize =
  do size@(w,h) <- gfxScreenSize
     fbinfo <- frameBufferInfo size
     if w==0 || h==0
       then return Nothing
       else do fgvar <- newRef (0::PixelValue)
	       clipvar <- newRef (((0,0),size)::Clip)
	       cursorvar <- newMVar ((w `div` 2,h `div` 2),(0,[]))
	       init fgvar clipvar cursorvar size fbinfo
  where
    init fgvar clipvar cursorvar size@(width,height) fbinfo =
      do show_cursor
	 return $ Just 
	   Graphics { screenSize = size::(Int,Int),
		      vbeInfo = getVbeInfo,
		      fbInfo = fbinfo,
		      pixelValue = pixelvalue,
		      setColor = setColor,
		      setPixelValue = writeRef fgvar,
		      setPixel = setPixel,
		      drawLine = drawLine,
		      fillRectangle = fillRectangle,
		      drawPixmap = drawPixmap,
		      setClipRect = setClip,
		      moveCursor = moveCursor,
		      hideCursor = hide_cursor,
		      showCursor = show_cursor
		    }
      where
	setColor = writeRef fgvar . pixelvalue

        pixelvalue :: (Int,Int,Int) -> PixelValue
        pixelvalue (red,green,blue) =
              c red   `shiftR` (16-redMaskSize fbinfo)
                      `shiftL` redFieldPosition fbinfo
	  .|. c green `shiftR` (16-greenMaskSize fbinfo)
                      `shiftL` greenFieldPosition fbinfo
	  .|. c blue  `shiftR` (16-blueMaskSize fbinfo)
                      `shiftL` blueFieldPosition fbinfo
           where
	     c = fromIntegral :: Int->PixelValue

        setClip xmin ymin xmax ymax =
          writeRef clipvar
            ((max 0 xmin,max 0 ymin),(min width xmax,min height ymax))

        readState m = 
          do fg <- readRef fgvar
	     clip <- readRef clipvar
	     m fg clip

        setPixel (x,y) = 
          readState $ \ fg clip ->
          when (inClip clip x y) $ withHiddenCursor $ setPixel' fg x y

        -- pre: (x,y) is within bounds
        setPixel' :: PixelValue -> Int -> Int -> H ()
        setPixel' fg x y = setPixel'' (toEnum (width*y+x)) fg

        setPixel'' :: Offset -> PixelValue -> H ()
	setPixel'' =
	  case bpp fbinfo of
	    1 -> \ p fg -> pokeElemOff b p (toWord8  fg)
	    2 -> \ p fg -> pokeElemOff b p (toWord16 fg)
	    3 -> \ p fg -> pokeElemOff b p (toWord24 fg)
	    4 ->           pokeElemOff b
{-
        setPixels' :: PixelValue ->  Int -> Int -> Int -> H ()
        setPixels' fg x y n = setPixels'' fg (width*y+x) n

	setPixels'' =
	    case bpp fbinfo of
	      1 -> \ fg -> pa b (fromIntegral fg)
	      2 -> \ fg -> pa b16 (fromIntegral fg)
	      3 -> \ fg -> pa b24 (toWord24 fg)
	      4 -> pa b32
          where pa b fg o n = pokeArray (U.advancePtr b o) (replicate n fg)
-}
        b = frameBuffer fbinfo
	--b16 = castPtr b::U.Ptr Word16
	--b24 = castPtr b::U.Ptr Word24
	--b32 = castPtr b::U.Ptr Word32

        getPixel' :: Int -> Int -> H PixelValue
        getPixel' x y = getPixel'' (toEnum (width*y+x))

        getPixel'' :: Offset -> H PixelValue
	getPixel'' =
	  case bpp fbinfo of
            1 -> \ p -> fromWord8  `fmap` peekElemOff b p
	    2 -> \ p -> fromWord16 `fmap` peekElemOff b p
	    3 -> \ p -> fromWord24 `fmap` peekElemOff b p
	    4 ->                          peekElemOff b

        fillRectangle :: Int->Int->Int->Int->H ()
        fillRectangle x0 y0 w h =
	  withHiddenCursor $
	  readState $ \ fg -> fillRectangle' fg . clip x0 y0 w h
           
        fillRectangle' :: PixelValue->Clip->H ()
        fillRectangle' fg ((x0,y0),(w,h)) =
	  when (w>0 && h>0) $
	    case bpp fbinfo of
	      1 -> fill pokeArray (toWord8 fg)
	      2 -> fill pokeArray (toWord16 fg)
	      --3 -> fill pokeArray (toWord24 fg)
	      4 -> fill pokeArray fg
	  where
            fill pokeArray fg =
                --withTempRegion bytes $ \ buf ->
		  --do fillRegion buf fg 
		     mapM_ (copyLine buf) [0..fromIntegral (h-1)] :: H ()
	      where
                --- Slow (not enough inlining/specialization, presumably):
                buf = listArray (1::Int,w) (repeat fg) `asTypeOf` (undefined::UArray Int e)
		copyLine buf y = pokeArray b (p*(o_x0y0+wi*y)) buf :: H ()
		--copyLine buf y = moveBytes b (p*(o_x0y0+wi*y)) buf 0 bytes
		o_x0y0 = fromIntegral (width*y0+x0)::Offset
		wi = fromIntegral width
		uw = fromIntegral w
		p = fromIntegral (bpp fbinfo)
		bytes = p*uw


        drawPixmap (x0,y0) (w,h) cmap transp pxls =
	  withHiddenCursor $
	  do cl <- readRef clipvar
             let ((x0',y0'),(w',h')) = clip x0 y0 w h cl
                 x1=x0'-x0
                 y1=y0'-y0
	     case transp of
	       Nothing ->
	         case bpp fbinfo of
                   1 -> fill pokeArray toWord8
                   2 -> fill pokeArray toWord16
                   --3 -> fill toWord24
                   4 -> fill pokeArray id 		 
                 where
                   fill pokeArray scale =
                       if w'>0
		       then --withTempRegion bytes $ \ buf ->
			    mapM_ (copyLine {-buf-}) [0..h'-1]
		       else return ()
		     where
		       copyLine {-buf-} dy =
			 do {-sequence_ [pokeElemOff buf (fromIntegral dx)
					 (scale(cmap!(pxls!(o_x1y1dy+dx))))
					 | let o_x1y1dy = x1+w*(y1+dy),
					   dx<-[0..w'-1]]
			    moveBytes b (p*(o_x0y0'+wi*fromIntegral dy)) buf
					0 bytes
                            -}
			    let buf = listArray (1,w')
				        [(scale(cmap!(pxls!(o_x1y1dy+dx))))
					   | let o_x1y1dy = x1+w*(y1+dy),
					     dx<-[0..w'-1]]
					`asTypeOf` (undefined::UArray Int e)
			    pokeArray b (p*(o_x0y0'+wi*fromIntegral dy)) buf :: H ()
		       o_x0y0' = fromIntegral (width*y0'+x0') :: Offset
		       wi = fromIntegral width
		       uw = fromIntegral w'
		       bytes = p*uw
		       p = fromIntegral (bpp fbinfo)
	       Just transpxl ->
		 sequence_ [setPixel' (cmap!pxl) (x0'+dx) (y0'+dy)
			   | dy<-[0..h'-1],dx<-[0..w'-1],
			     let pxl=pxls!(x1+dx+w*(y1+dy)),
			     pxl/=transpxl]

{-


        drawPixmap (x0,y0) (w,h) cmap transp pxls =
	  withHiddenCursor $
	  do cl <- readRef clipvar
             let ((x0',y0'),(w',h')) = clip x0 y0 w h cl
                 x1=x0'-x0
                 y1=y0'-y0
	     case transp of
	       None ->
		 sequence_ [setPixel' (cmap!(pxls!(x1+dx+w*(y1+dy))))
				      (x0'+dx) (y0'+dy)
			   | dy<-[0..h'-1],dx<-[0..w'-1]]
	       Just transpxl ->
		 sequence_ [setPixel' (cmap!pxl) (x0'+dx) (y0'+dy)
			   | dy<-[0..h'-1],dx<-[0..w'-1],
			     let pxl=pxls!(x1+dx+w*(y1+dy)),
			     pxl/=transpxl]

-}	

        clip :: Int -> Int -> Int -> Int -> Clip -> Clip 
        clip x0 y0 w h ((xmin,ymin),(xmax,ymax)) = ((x0',y0'),(w',h'))
          where
	    x0' = max xmin x0
	    y0' = max ymin y0
	    x1  = min (xmax-1) (x0+w-1)
	    y1  = min (ymax-1) (y0+h-1)
	    w' = x1-x0'+1
	    h' = y1-y0'+1

        inClip :: Clip -> Int -> Int -> Bool
        inClip ((xmin,ymin),(xmax,ymax)) x y =
           xmin<=x && x<xmax && ymin<=y && y<ymax

        drawLine (x1,y1) (x2,y2) =
	    if x1==x2
	    then let (y0,h) = order y1 y2
		 in fillRectangle x1 y0 1 h -- slow
	    else if y1==y2
		 then let (x0,w) = order x1 x2
		      in fillRectangle x0 y1 w 1
		 else withHiddenCursor $ readState $ slantedline
          where
	    order v1 v2 = if v1<=v2 then (v1,v2-v1+1) else (v2,v1-v2+1)
            slantedline fg clip =
		if dx>dy
		then sequence_
		       [setPixel (x1+sx*i) (y1+sy*(i*dy `div` dx))|i<-[0..dx]]
		else sequence_
		       [setPixel (x1+sx*(i*dx `div` dy)) (y1+sy*i)|i<-[0..dy]]
              where
                (dx,sx) = distdir x1 x2
		(dy,sy) = distdir y1 y2
		setPixel x y = when (inClip clip x y) $ setPixel' fg x y

            distdir v1 v2 = (abs d,signum d) where d=v2-v1

        -------

	withHiddenCursor m = do hide_cursor; m; show_cursor

        moveCursor x y =
          withHiddenCursor $ do (_,b) <- takeMVar cursorvar
			        putMVar cursorvar ((x,y),b)

        show_cursor =
          do (p,(oldcnt,saved)) <- takeMVar cursorvar
	     let cnt=oldcnt+1
             if cnt==1
		then do shape <- draw_cursor p
			putMVar cursorvar (p,(cnt,shape))
		else putMVar cursorvar (p,(cnt,saved))

        hide_cursor =
          do (p,(oldcnt,saved)) <- takeMVar cursorvar
	     let cnt=oldcnt-1
             if cnt==0
		then do erase_cursor saved
			putMVar cursorvar (p,(cnt,[]))
		else putMVar cursorvar (p,(cnt,saved))

        erase_cursor = mapM_ restorePixel
	  where restorePixel ((x,y),fg) = setPixel' fg x y

        draw_cursor (x0,y0) = concat `fmap` mapM drawPixel cursorshape
          where
	    drawPixel (dx,dy) =
		if inClip screenclip x y
		then do fg <- getPixel' x y
			setPixel' curfg x y
			return [((x,y),fg)]
		else return []
	      where x = x0+dx
		    y = y0+dy

        curfg = pixelvalue (0xffff,0,0)
        cursorshape = nub $ concat [[(i,i),(-i,i)]|i<-[-4..4]]
        screenclip = ((0,0),size)

---

toWord8    = fromIntegral :: PixelValue -> Word8
toWord16   = fromIntegral :: PixelValue -> Word16

fromWord8  = fromIntegral :: Word8  -> PixelValue
fromWord16 = fromIntegral :: Word16 -> PixelValue
