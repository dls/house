module Badget(badgetF{-,gadgetDisplay-}) where

import AllFudgets hiding (SPms)
import List(isSuffixOf)
import qualified Gio

-- badgetF: a bad gadget implementation :-)

--badgetF :: Gadget -> F a b

--gadgetDisplay cmdCh evCh  = fudIO cmdCh evCh $ shellF "Gadgets" badgetF

badgetF :: F Gio.GCommand Gio.GEvent
badgetF =
    windowF [layoutRequestCmd (plainLayout (Point 1000 700) True True),
	     XCmd $ ChangeWindowAttributes [CWEventMask eventmask,
	                                    CWBitGravity NorthWestGravity]]
            ctrl0
  where
  eventmask = [{-PointerMotionMask,-}ExposureMask,ButtonPressMask,ButtonReleaseMask,KeyPressMask]
  ctrl0 = wCreateGC rootGC [] ctrl00
  ctrl00 gc = ctrl where
   tob c = putHigh c ctrl
   ctrl = getK $ \msg -> case msg of
     High bc -> case bc of
		  Gio.MC _ -> ctrl
		  Gio.KC _ -> ctrl
		  Gio.SC scs -> todraws gc scs ctrl00
     Low (XEvt e) -> case e of
       KeyEvent _ _ _ _ Pressed _ _ [k] -> 
	   tob (Gio.KE (Gio.KeyPress k))
       ButtonEvent _ (Point x y) _ _ Pressed (Button b) -> 
	   tob (Gio.ME (Gio.MouseClick x y b))
       ButtonEvent _ (Point x y) _ _ Released (Button b) -> 
	   tob (Gio.ME (Gio.MouseUnClick x y b))
       MotionNotify _ (Point x y) _ _ -> ctrl --tob (Gio.ME (MouseMove x y))
       Expose (Rect (Point x y) (Point xs ys)) i -> 
	   tob (Gio.SE (Gio.Expose ((x,y),(xs+x,ys+y))))
       _ -> ctrl
     _ -> ctrl
   todraws gc [] c = c gc
   todraws gc (d:ds) c = todraw gc d $ \ gc' -> todraws gc' ds c
   todraw gc d c = case d of
      Gio.DrawSetPixel p@(x,y) pxl ->
        --appendChanK "stderr" ("DrawSetPixel "++show (x,y)++" "++show pxl++"\n") $
	changeGC gc [GCForeground (Pixel (fromIntegral pxl))] $ \ gc ->
	putLow (wDrawPoint gc (pP x y)) $ c gc
	--putLow (wDrawLine gc (lL x y x y)) $ c gc
	--draw [wDrawLine gc (gl2l (p,p))]
        --draw [wDrawPoint gc (pP x y)]
      Gio.DrawLine l ->
         --appendChanK "stderr" ("DrawLine "++show l++"\n") $
         draw [wDrawLine gc (gl2l l)]
      Gio.DrawFilledRectangle l -> draw [wFillRectangle gc (gline2rect l)]
      Gio.DrawText p s -> draw [wDrawString gc (gp2p p) s]
      Gio.DrawSetColour rgb ->
 	allocColorPixel defaultColormap (grgb2rgb rgb) $ \ pxl ->
	changeGC gc [GCForeground pxl] c
      Gio.DrawSetPixelValue pxl ->
        --appendChanK "stderr" ("DrawSetPixelValue "++show pxl++"\n") $
	changeGC gc [GCForeground (Pixel (fromIntegral pxl))] c
      Gio.DrawSetFont fn ->
	convFontK (fontAlts fn) $ \ fdata ->
 	changeGC gc [GCFont (fdFontId fdata)] c
      Gio.DrawCircle (p,r) -> draw [wDrawCircle gc (gp2p p - diag r) (2*r)]
      Gio.DrawFilledCircle (p,r) -> draw [wFillCircle gc (gp2p p - diag r) (2*r)]
      Gio.DrawFilledTriangle (p1,p2,p3) ->
	draw [wFillPolygon gc Convex CoordModeOrigin (map gp2p [p1,p2,p3])]
      Gio.DrawSetClip r -> xcommandsK [SetRegion gc (line2rect.gl2l $ r)] $ c gc -- not gline2rect !!
      _ -> c gc
    where draw cmds = putLows cmds $ c gc

changeGC gc gcattrs c =
  xcommandK (ChangeGC gc gcattrs) $
  --wCreateGC gc gcattrs $ \ gc' ->
  --putK (Low (FreeGC gc)) $
  c gc

gl2l ((x1,y1),(x2,y2)) =lL x1 y1 x2 y2

gline2rect = flip growrect (pP 1 1) . line2rect . gl2l
   -- line2rect requires that the first point is the upper left corner...

gp2p (x,y) = Point x y
grgb2rgb (r,g,b) = RGB r g b

fontAlts fn =
    if "-iso8859-1" `isSuffixOf` fn
    then [fn,fnu,"fixed"]
    else [fn,"fixed"]
  where
    fnu = take (length fn-10) fn++"-iso10646-1"
