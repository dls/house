-- A seriously cut-down version of UserProc.hs 
-- for proof development.

-- To start with, we'll just do fixPage, with
-- a somewhat simplified Uproc structure.

module UserProcModel where

import HModel.SomeH
import Numeric(showHex)
import Data.Word(Word8, Word32)
import Kernel.AOutModel
       
data UProc = UProc
	     {aout        :: AOut,
	      myPutStr    :: String -> H (),
	      brk	  :: VAddr,
              stackRegion :: VRegion,
	      pmap        :: PageMap,
	      ownedPages  :: [PhysPage]
	      {- context  :: Context  -}
	     }  

maxStackSize :: Word32
maxStackSize  = 0x100000

buildUProc :: (String -> H()) -> Image -> H UProc
buildUProc putStr aoutImage =
     do aout  <- buildAOut aoutImage
        let brk = pageCeiling (snd $ bssRegion aout)
        let endStack = snd userRegion
        let startStack = endStack - maxStackSize
	Just pmap  <- allocPageMap  -- die here if out of space
        return UProc
	       {aout        = aout,
                myPutStr    = putStr,
	        brk         = brk,
	        stackRegion = (startStack,endStack),
	        pmap        = pmap,
	        ownedPages  = []
		{- context     = zeroContext{eip = entry aout,esp = endStack} -}
               }
       
fixPage :: UProc -> VAddr -> H (Either UProc String)
fixPage uproc vaddr | vaddr `inVRegion` (codeRegion $ aout uproc)
  = do page <- setupPage uproc vaddr False
       copyPage page (codeBytes (aout uproc) .
                                  (+fromIntegral ((pageFloor vaddr) - (fst $ codeRegion $ aout uproc))))
       return $ Left uproc{ownedPages=page:(ownedPages uproc)}
fixPage uproc vaddr | vaddr `inVRegion` (dataRegion $ aout uproc)
  = do page <- setupPage uproc vaddr True
       copyPage page (dataBytes (aout uproc) .
                                  (+fromIntegral ((pageFloor vaddr) - (fst $ dataRegion $ aout uproc))))
       return $ Left uproc{ownedPages=page:(ownedPages uproc)}
fixPage uproc vaddr | vaddr `inVRegion` (fst $ bssRegion $ aout uproc,brk uproc)
  = do page <- setupPage uproc vaddr True
       zeroPage page
       return $ Left uproc{ownedPages=page:(ownedPages uproc)}
fixPage uproc vaddr | vaddr `inVRegion` (stackRegion uproc)
  = do page <- setupPage uproc vaddr True
       zeroPage page
       return $ Left uproc{ownedPages=page:(ownedPages uproc)}
fixPage uproc vaddr | otherwise 
  = return $ Right ("Fatal page fault at " ++ (showHex vaddr ""))

setupPage :: UProc -> VAddr -> Bool -> H PhysPage
setupPage uproc vaddr writable
 = do Just page <- allocPhysPage
      let pi    = PageInfo {physPage=page,
                            writable=writable, dirty=False, accessed=False}
      setPage (pmap uproc) vaddr (Just pi)
      return page

zeroPage :: PhysPage -> H ()
zeroPage physPage = 
   sequence_ [setPAddr (physPage,off) 0 | off <- [minBound..]]

copyPage :: PhysPage -> Image -> H ()
copyPage physPage src =
   sequence_ [setPAddr (physPage,off) =<< src (fromIntegral off) | off <- [minBound..]] 

-- round down to nearest whole number multiple of page size
pageFloor :: VAddr -> VAddr
pageFloor vaddr = vaddr `div` (fromIntegral pageSize) * (fromIntegral pageSize)

-- round up to nearest whole number multiple of page size
pageCeiling :: VAddr -> VAddr 
pageCeiling vaddr = ((vaddr - 1) `div` (fromIntegral pageSize) + 1) * (fromIntegral pageSize)


