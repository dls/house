-- From InternetLib by Thomas Hallgren,
-- http://www.cs.chalmers.se/~hallgren/InternetLib/
module GIFops where
import GIF
import Data.Array.Unboxed(listArray)
import Data.Word(Word8)

apRasterData f gif = gif { data_blocks=map db (data_blocks gif) }
  where
    db = either Left (Right . im)
    im image = image { raster_data = f (image_descriptor image) (raster_data image) }

decompressRasterData decompr = apRasterData rd
  where
    rd id = either (Right . decompr (pixelcount id)) Right

compressRasterData compr = apRasterData rd
  where
    rd id = either Left (Left . compr (pixelcount id))

pixelcount :: ImageDescriptor -> Int
pixelcount id = fromIntegral (iwidth id)*fromIntegral (iheight id)

pixels2array :: Int -> [Word8] -> PixelArray
pixels2array n ps = listArray (0,n-1) ({-map fromIntegral-} ps)
