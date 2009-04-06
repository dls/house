-- From InternetLib by Thomas Hallgren,
-- http://www.cs.chalmers.se/~hallgren/InternetLib/
module GIFparser(parseGIF,parseGIFs,sizeOfGIF) where
import GIF
import ParsOps2
import Utils2(bit,bits)
import Data.Array.Unboxed(listArray)
import Data.Word(Word8)

default(Int)

--import Trace
--tr s = trace s $ return ()
tr s = return ()

sizeOfGIF = parse gifSizeP
  where
    gifSizeP =
      do signatureP
         sd <- screenDescriptorP
	 theRest
	 return (swidth sd,sheight sd)

parseGIF = parse gifP
parseGIFs = parse gifsP

gifsP = some gifP

gifP =
   do h <- signatureP
      sd <- screenDescriptorP
      gmap <- optColorMapP (hasGlobalMap sd) (sbitsPerPixel sd)
      ds <- dataBlocksP
      charP ';'
      --r <- theRest -- allow trailing garbage
      return (GIF h sd gmap ds)

signatureP = do s <- fmap (map (toEnum.fromIntegral)) (tokens 6)
		if s `elem` ["GIF87a","GIF89a"]
		   then return s
		   else failP "Missing GIF87a or GIF98a signature"

optColorMapP b n =
  if b
  then fmap Just (colorMapP n)
  else tr "no colormap" >> return Nothing

colorMapP size = tr ("colorMapP "++show n) >> repeatP n colorP
  where n = 2^size

colorP =
  do r <- byteP
     g <- byteP
     b <- byteP
     return (r,g,b)

screenDescriptorP =
  do w <- wordP
     h <- wordP
     opts <- byteP
     bg <- byteP
     ar <- byteP
     return (SD w h (bit 7 opts) (bitcount 4 opts) (bit 3 opts) (bitcount 0 opts) bg ar)

bitcount p byte = 1+bits p 3 byte

dataBlocksP = many dataBlockP

dataBlockP = eitherP extensionBlockP imageP

imageP = 
  do charP ','
     id <- imageDescriptorP 
     lmap <- optColorMapP (hasLocalMap id) (ibitsPerPixel id)
     rd <- rasterDataP
     return (Image id lmap rd)

imageDescriptorP =
  do l<-wordP
     t<-wordP
     w<-wordP
     h<-wordP
     opts<-byteP
     return (ID l t w h (bit 7 opts) (bit 6 opts) (bitcount 0 opts))
  
rasterDataP = fmap Left compressedBlocksP

compressedBlocksP =
  do c <- byteP
     bs <- blocksP
     return (CB c bs)

extensionBlockP =
  do charP '!'
     c <- byteP
     bs <- blocksP
     return (EB c bs)

blocksP =
  do cnt <- byteP
     if cnt == 0
      then return []
      else (:) `fmap` byteArrayP cnt `ap` blocksP

byteArrayP :: Int -> Parser Word8 ByteArray
byteArrayP n =
  do a <- listArray (1,n) `fmap` tokens n
     seq a $ return a

byteP :: Parser Word8 Byte
byteP = fmap fromIntegral token

wordP = 
  do lo <- byteP
     hi <- byteP
     return (hi*256+lo)

charP :: Char -> Parser Word8 Char
charP c = do lit (fromIntegral (fromEnum c))
	     return c
