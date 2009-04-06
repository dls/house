module DrawGIF(drawGIF) where
import GIF
import GIFdecompress
import GadgetsPrelude
import Area(subtractArea)
--import Data.Array
import Data.Maybe(fromJust)
import Data.Array.Unboxed as U
import Data.Word(Word8)

drawGIF :: (RGB -> PixelValue) -> GIF.File -> DrawFun
drawGIF pv = drawGIF' pv . decompressGIF

drawGIF' pixelValue (GIF sig scr cmaplist blocks) size area = 
    optdrawbg ++ drawBlocks blocks
  where
    globalcmap = fmap (convColormap pixelValue) cmaplist
    optbg = fmap (!bgpxl) globalcmap
    bgpxl = fromIntegral (background scr)
    optbgpxl = fmap (const bgpxl) optbg

    -- Reduce flicker by not drawing the background if it is completely covered.
    optdrawbg =
	if covered scr blocks
	then concatMap (drawbg optbg) (subtractArea ((0,0),(w,h)) [area])
	else drawbg optbg area
      where SD {swidth=w,sheight=h} = scr

    drawbg Nothing   _ = []
    drawbg (Just bg) r = DrawSetPixelValue bg:fillrect r

    drawBlocks = concatMap drawBlock
    drawBlock = either drawEB drawImage
    drawEB _ = [] -- hmm

    drawImage (GIF.Image (ID x0 y0 w h _ i _) cmaplist (Right ps)) =
        [DrawPixmap (Pixmap cmap Nothing (w,h) ps) (x0,y0)]
{-
        zipWith (setPixel . (cmap!))
		   (U.elems ps)
		   [(x,y)|y<-take h [y0..],x<-take w [x0..]]
-}
      where
        cmap = maybe (fromJust globalcmap) (convColormap pixelValue) cmaplist


    covered SD {swidth=sw,sheight=sh} bs = any coveredByBlock bs
      where
        coveredByBlock = either (const False) coveredByImage
	coveredByImage (GIF.Image (ID 0 0 iw ih _ _ _) _ _) = (iw,ih)==(sw,sh)
	coveredByImage _ = False


setPixel c p = DrawSetPixel p c

convColormap ::  (RGB -> PixelValue) -> GIF.ColorMap -> UArray Word8 PixelValue
convColormap pv xs = listArray (0,fromIntegral (length xs-1)) (map convRGB xs)
  where convRGB (r,g,b) = pv (257*r,257*g,257*b)
