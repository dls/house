-- From InternetLib by Thomas Hallgren,
-- http://www.cs.chalmers.se/~hallgren/InternetLib/
module DescribeGIF where
import GIF
import Data.Array.Unboxed(bounds)

describeGIF = unlines . descr
  where
    descr (GIF sig sd gcm dbs) =
      sig:descrSD sd++descrCM gcm++descrDataBlocks dbs

    descrSD (SD {swidth=swidth,sheight=sheight,background=background}) =
      map unwords
        [["Screen size:",show swidth,"x",show sheight],
	 ["Blackground color index:",show background]
	]

    descrCM = (:[]) . maybe "No colormap" (("Color map of size "++).show.length)

    descrDataBlocks = concatMap descrDataBlock

    descrDataBlock = either descrExt descrImage

    descrExt eb =
      map unwords $
      case eb of
        EB fc fd -> [["Extension block, function code:",show fc,
	             "size:",sbl fd]]
	GCE dm ui tcf dt tci ->
	  [["Graphic Control Extension:"],
	   ["\tdisposal method:",show dm,
	    "delay time:",show dt]++
	    if tcf then ["transparent color index:",show tci] else []]
	Comment s -> [["Comment:",s]]
	AE ai ac ad  -> [["Application Extension:",ai,"data size:",sbl ad]]

    descrImage (Image id lcm rd) =
      zipWith (++) ("Image:\t":repeat "\t") (descrID id++descrCM lcm)

    descrID (ID {left=x,top=y,iwidth=w,iheight=h,interlace=lace,ibitsPerPixel=bpp}) =
      map unwords
        [["pos",show x,show y,"size",show w,"x",show h,
	 "depth",show (bpp+1),"interlace:",show lace]]

    sbl = show . sum . map alength

    alength a = (hi-lo+1)
      where (lo,hi) = bounds a
