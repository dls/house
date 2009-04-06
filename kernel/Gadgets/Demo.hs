module Demo where

import Gadgets hiding (textSize)
import HOpBadget(gadgetDisplay,screenSize,findFont)

import Multicount
import Explode
import Graph
import Bristol
import Calc
import Updown
import Escher
import TerminalTest(tt2')
import H.Monad(liftIO,runH)
import Util.Font(double,textSize)
import DrawGIF
import GIF
--import GIFparser(parseGIFs)
import GIFdecompress(decompressGIF)
import Kernel.Driver.IA32.VbeGraphics(pixelValue)
import Keys

default(Int)

gadgetsDemo gfx fonts1 gifss shell loadGIFs =
    liftIO $
    posGo size (\ c e -> runH $ gadgetDisplay gfx fonts c e) demos
  where
    fonts = fonts1 ++ [((n,2*h),double f)|((n,h),f)<-fonts1]
    size = screenSize gfx
    demos = [((0,0),demowall gfx fonts gifss size shell loadGIFs)]
{-
            [-- counters,
	     --gedit,bristol,
	     ((512,450),calc),
	     ((512,10),updown),
	     ((650,10),explode 4 4)
	     {-,escher-}
	    ]
-}

demowall gfx fonts gifss (wi,he) shell loadGIFs = (mywall,"House")
  where
    mywall =
      wire $ \w ->
      tx (op w) (Just (10,10),window (control (op w)) "Start") $
      wall' (width wi.height he.childrenAt (150,10).
	     picture (colourbox (col "light grey")))
	    (ip w)

    terminalSmall = terminal' "small" fn
      where
	fn = "fixed"

    terminalBig = terminal' "big" fn
      where
	fn = "*-courier-medium-r-*-14-*"
	--fn = "*-courier-bold-r-*-24-*"

    terminal' variant fn = (g,"House Shell " ++"("++variant++")")
      where
        g = ioProcess (runH shell)
                      (\ (outCh,inCh) -> tt2' ch outCh (Just inCh))
        ch = pickfont fonts fn

    gifDisp (path,[gif]) = (gifpaper nci gif,path)
    gifDisp (path,gif:gifs) =
        (slideshow imgsize gif gifs,path++" ("++show (1+length gifs)++")")
      where
        imgsize = (maximum ws,maximum hs)
          where (ws,hs) = unzip (map gifsize (gif:gifs))

    showSlides = (slideshow, "Slideshow")
      where
        slideshow = wire $ \ slidesw ->
		    setGapSize 5 $
		      getslides (op slidesw)
                        <|>
                      slideshow' (ip slidesw) (800,600) emptyGIF []

        getslides slidesw =
            wire $ \ w ->
	    spawn (claim (ip w) $ fetch (ip w)) $
            entry' (width 240.height 20) (op w) "Path"
	  where
            fetch i = rx [from i $ \ path ->
			  ioProcess (loadGIFs path) $ \ s ->
			  tx slidesw (prefix path $ parseSlides s) $
			  fetch i]
		          (rxFail "getslides")::Component
	    prefix path = either (Left . ((path++": ")++)) Right
            parseSlides = either Left nonEmpty
	    nonEmpty [] = Left "empty slide show"
	    nonEmpty (gif:gifs) = Right (gif,gifs)

    slideshow = slideshow' nci
    slideshow' slidesw imgsize gif gifs =
        wire $ \ picw ->
	wire $ \ ctrlw ->
	spawn (forceAll gifs end::Component) $
	spawn (ctrl (op picw) (ip ctrlw) gif gifs) $
	setGapSize 5 $
	buttons (op ctrlw) <|> gifpaper' imgsize (ip picw) gif
      where
        forceAll gifs cont = foldr decode cont gifs
	  where decode gif = seq (gif==gif)

	ctrl outpic inctrl gif gifs =
            claim inctrl $
	    claim slidesw $
	    loop ([],gif,gifs)
	  where
	    loop state@(behind,_,_) =
		rx [from inctrl action,from slidesw newslides]
		   (rxFail "slideshow")::Component
	      where
		action lbl = optshow (move lbl state)

                newslides (Left msg) =
                    tx outpic pic $ loop state
                  where pic s a = blank s a ++ text fn co msg s a
		newslides (Right (gif,gifs0)) =
		    let gifs = map decompressGIF gifs0
                    in spawn (forceAll gifs end::Component) $
		       show ([],decompressGIF gif,gifs)

		optshow state'@(behind',_,_) =
		    if length behind/=length behind' -- did we move?
		    then show state'
		    else loop state

                show state'@(_,current,_) =
		    tx outpic (drawGIF pv current) $ loop state'

	    move "|<" = to_first
	    move "<<" = go_back 10
	    move "<" = go_back 1
	    move ">" = go_forward 1
	    move ">>" = go_forward 10
	    move ">|" = to_last

	    move " "  = go_forward 1
	    move "\b" = go_back 1
	    move _    = id

	    to_first = go_back (-1) -- trick coding!
	    to_last = go_forward (-1) -- trick coding!

	    go_back 0 state = state
	    go_back n (prev:behind,current,ahead) =
		go_back (n-1) (behind,prev,current:ahead)
	    go_back _ state = state

	    go_forward 0 state = state
	    go_forward n (behind,current,next:ahead) =
		go_forward (n-1) (current:behind,next,ahead)
	    go_forward _ state = state

	buttons o =
	    beside $ keys o : map arrow ["|<","<<","<", ">", ">>",">|"]
	  where
	    arrow lbl = button' changes o lbl
	      where changes = picture (text fn co lbl) . width 25 . height 20

	fn = "fixed"
	co = col "black"

    pv = pixelValue gfx

    gifpaper inp gif = gifpaper' (gifsize gif) inp gif

    gifpaper' (w,h) inp gif = paper' changes
      where
	changes = width w . height h . picture (drawGIF pv gif) .
		  paperNoBg . pictureIn inp

    gifsize GIF { screen_descriptor=SD{swidth=w,sheight=h}} = (w,h)

    control o =
	setGapSize 5 $
        vb ([terminalBig,terminalSmall,updown,calc,explode 4 4,escher,counters,
	     gedit,bristol]
	    ++map (gifDisp.apSnd (map decompressGIF)) gifss
	    ++[showSlides])
      where
        vb = above . map b

	b (g,title) =
	    button' (width 120.height 20.picture (text fn co title))
		    o
		    (Nothing,window g title)
          where
	    fn = "fixed"
	    co = col "black"


pickfont fonts fn = fontwmetrics fn (textSize (findFont fonts fn))

apSnd f (x,y) = (x,f y)

emptyGIF = GIF { signature="GIF87a",
		 screen_descriptor=sd,
		 global_color_map=Just [(255,255,255),(0,0,0)],
		 data_blocks=[] }
  where
    sd = SD { swidth=800,
	      sheight=600,
              hasGlobalMap=True,
	      colorResolution=7,
	      sortFlag=False,
	      sbitsPerPixel=0,
	      background=0,
	      aspectRatio=0 }


entry o = entry' id o

entry' changes o init = 
    wire $ \ w ->
    spawn (changeToConst init (ip w) o) $
    enterable' (editorOutput (op w).changes) init
