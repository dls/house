module GfxBenchmark(timeLimit,gfxBenchmark) where
--import Kernel.Driver.CMOS
import Kernel.Driver.IA32.VbeGraphics as VBE
import Kernel.Timer(readTimer)

timeLimit t ms =
  do t0 <- readTimer
     let loop n [] = return n
	 loop n (m:ms) = do m
			    t' <- readTimer
			    if t'-t0<t
			       then loop (n+1) ms
			       else return n
     loop 0 ms
			
gfxBenchmark gfx fonts = map drawSomething [20..700]
  where
    font = snd (head fonts)
    drawSomething y = drawTextBox gfx font bg fg (y `div` 2) y s
      where
	s = show y++' ':[' '..'z']
	bg = (0,0,80*y)
	fg = (65535,65535,65535-80*y)

{-
getCMOSSeconds =
  do (year, month, day, hour, min, sec) <- getRawTime
     return $ sec+60*(min+60*hour)
-}
