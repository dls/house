-----------------------------------------------------------------------
module Hovel(main) where

import Data.Char(ord)
import H.Monad(H,runH,liftIO,trace)
import Kernel.AOut(Image)
import Kernel.UserProc(UProc,buildUProc,execUProc)
import Kernel.GrubModules(pickModule,moduleRegion)  -- to get images from grub modules
import H.MemRegion(peekByteOff)                     -- ditto

-----------------------------------------------------------------------
       
-- For verification purpoes, we only need to consider the behavior of exec.
-- Note that this does not depend on the actual images provided, so
-- there's no need to prove anything about the aout format processing.

exec :: Image -> [Image] -> H ()
exec tracedImage otherImages =
   do tracedProc <- buildUProc tracedPutStr tracedImage
      otherProcs <- mapM (buildUProc putStr) otherImages
      schedule $ tracedProc:otherProcs    

-- A very simple round-robin scheduler.
schedule :: [UProc] -> H ()
schedule [] = schedule []
schedule (uproc:uprocs) = 
  do r <- execUProc uproc
     case r of
       Left uproc' -> schedule $ uprocs ++ [uproc']
       Right msg -> 
         do putStrLn ("EXIT:" ++ msg) -- just for testing
            schedule uprocs

tracedPutStr :: String -> H()
tracedPutStr s =
   do trace s
      putStr s  -- just for testing

---------------------------------------------------------------------
-- The main program is just to test the framework; it 
-- loads and runs aout images from grub modules,
-- and it displays output from all processes as it is generated.

main :: IO ()
main = runH
     $ do initVideo
          putStr "Welcome to Hovel!\n\n"
          helloImage <- loadImage "hello"  
	  divImage <- loadImage "div"
	  loopImage <- loadImage "loop"
  	  exec helloImage [loopImage,helloImage,divImage]
          putStr "Terminating!\n"
          let halt = putStr "" >> halt in halt

-- getting images from grub modules
loadImage :: String -> H Image
loadImage file =
   do findex <- pickModule putStrLn file
      case findex of
        Nothing -> do putStrLn ("No module: " ++ file)
 	              error "Fatal Error"
        Just i  -> do reg <- moduleRegion i
                      return $ peekByteOff reg

-- Quick and dirty interface to raw video: ----------------------------

foreign import ccall unsafe "video.h" init_video :: IO ()
foreign import ccall unsafe "video.h" putch      :: Int -> IO ()

initVideo  = liftIO init_video
putCh c    = liftIO (putch (ord c))
putStr     = mapM_ putCh
putStrLn s = putStr s >> putStr "\n"

-----------------------------------------------------------------------


