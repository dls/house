-------------------------------------------------------------------------------
-- Decode an a.out image into an AOut structure
--
-- Assumes a.out image has been linked with text @ 0x10000000, 
--         data @ next page boundary after text, 
--	   and bss @ next page boundary after data.
-------------------------------------------------------------------------------

module Kernel.AOut(AOut(..),Image,Offset,VRegion,
                   inVRegion, userRegion, buildAOut) where

import Data.Word(Word8, Word32)
import Data.Bits
import Kernel.Bits(catBits)
import H.Monad(H)
import H.VirtualMemory(VAddr)

-- full static info about a.out file
data AOut = AOut 
	     {entry      :: VAddr, 
	      codeRegion :: VRegion,
	      dataRegion :: VRegion,
	      bssRegion  :: VRegion,
	      codeBytes  :: Image,  -- return n'th code byte
	      dataBytes  :: Image   -- return n'th data byte
	     } 

instance Show AOut where
  showsPrec _ aout s = s ++ 
               "Aout{entry=" ++ show (entry aout) ++ 
	        ",codeRegion=" ++ show (codeRegion aout) ++
	        ",dataRegion=" ++ show (dataRegion aout) ++ 
	        ",bssRegion=" ++ show (bssRegion aout) ++
               "}"

type Image = Offset -> H Word8  -- abstract accessor into image 
type Offset = Word32      -- zero-based offset into the executable (a.out) image
type VRegion = (VAddr, VAddr)   
                  -- by convention, interval is closed on left but open on right
      
{- a.out header offsets -}
headerLen     = 0x20
headerInfo    = 0x00
headerTextLen = 0x04 -- includes header AND text
headerDataLen = 0x08
headerBssLen  = 0x0C
headerEntry   = 0x14

userRegion :: VRegion
userRegion = (0x10000000,0x40000000)

imageWord32 image offset =
   (catBits . reverse) `fmap` mapM image [offset..offset+3]
   -- little endian

buildAOut :: Image -> H AOut
buildAOut aoutImage =
     do entry   <- imageWord32 aoutImage headerEntry 
	lenText <- imageWord32 aoutImage headerTextLen 
        lenData <- imageWord32 aoutImage headerDataLen
	lenBss  <- imageWord32 aoutImage headerBssLen
	let endCode = fst userRegion + lenText - fromIntegral headerLen
                      -- a.out QMAGIC weirdness
	let endData = endCode + lenData
        let endBss = endData + lenBss
        return AOut
	       {entry      = entry,
	        codeRegion = (fst userRegion,endCode),
                dataRegion = (endCode,endData),
                bssRegion  = (endData,endBss),
		codeBytes  = aoutImage . (fromIntegral headerLen+),
                dataBytes  = aoutImage . (lenText+)}

inVRegion :: VAddr -> VRegion -> Bool
inVRegion x (s,e) = x >= s && x < e
