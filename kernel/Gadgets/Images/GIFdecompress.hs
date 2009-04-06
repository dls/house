-- From InternetLib by Thomas Hallgren,
-- http://www.cs.chalmers.se/~hallgren/InternetLib/
module GIFdecompress(decompressGIF,decompressGIFLZW) where
import GIF
import GIFops(decompressRasterData,pixels2array)
import BitStream(btake,bitstream,BitStream)
import Data.Array
import Data.Array.Unboxed as U(elems)
import Data.Word(Word8)

--import Debug.Trace(trace)
--trace x y = y
--tr x = trace (show x) x
--tr' s x = trace (s++show x) x

decompressGIF = decompressRasterData decompr
  where
    decompr n (CB c bs) =
      pixels2array n . decompressGIFLZW c 
      -- . map fromIntegral . concat . map U.elems 
      $ bs

decompressGIFLZW c = lzwdecompress c . bitstream

lzwdecompress root_code_size bs = pixels
  where
    pixels  = [ p | Right p<-result ]
    updates = [ u | Left  u<-result ]
    result = decompr1 (inittable root_code_size updates) bs
    --decompr1 table End = [] -- to avoid looping if the EOI code is missing
    decompr1 table bs =
      case get_next table bs of
        (c,bs) | c==clear table -> decompr2 table bs
	       | c==eoi table -> []
	       | otherwise -> output (dget table c) $ decompr table c bs

    decompr2 table bs = startnewtable $ decompr1 (nexttable root_code_size table) bs

    --decompr table old End = [] -- to avoid looping if the EOI code is missing
    decompr table old bs =
      case get_next table bs of
	(c,bs) | c==clear table -> decompr2 table bs
	       | c==eoi table -> []
	       | otherwise ->
	    case dlookup c table of
	      Just t -> output t $ update ott $ decompr (add table) c bs
                where ott = extend ot t
	      Nothing -> output t $ update t $ decompr (add table) c bs
		where t = extend ot ot
	  where
	    ot = dget table old

{- --Old:
extend ot t = ot++first t
output t r = --trace ("output "++show t) 
	     (map Right t++r)
--}

--{- -- New:
extend (BS ot) (BS t) = BS (last t:ot)
output (BS [])     r = r
output (BS (t:ts)) r = output (BS ts) (Right t:r)
--}

startnewtable r = --trace "newtable "
	     Left Nothing:r
update t r = --trace ("update "++show t) 
	     Left (Just t):r

add' = add
--add' table s = trace ("add "++show s++" = "++show (next table)) $ add table s


first (x:_) = [x]

type Output = Either (Maybe Bytes) Byte
-------------------------------------------------------------------------------

data Table = T { code_size,maxcode,clear,eoi,next:: !Int,
                 entries:: Map }

type Map = [Array Int Bytes]

newtype Bytes = BS [Word8] -- stored in reverse order
type Code = Int

max_code_size=12
max_maxcode=2^max_code_size-1

newtable root_code_size tables =
    T code_size maxcode clear eoi next tables
  where
    code_size = root_code_size+1
    clear=2^root_code_size
    eoi=clear+1
    next=clear+2
    maxcode=2*clear-1

--inittable :: Int -> Table
inittable root_code_size updates =
    newtable root_code_size (maps 0 updates)
  where
    clear=2^root_code_size
    maps n us = lazyArray n (clear+2,max_maxcode) us1:maps (n+1) us2
      where (us1,us2) = split us

nexttable root_code_size t@T{entries= _:as} =
   --trace "nexttable" $
   newtable root_code_size as

split [] = ([],[])
split (Nothing:us) = ([],us)
split (Just u:us) = (u:us1,us2) where (us1,us2) = split us
--split (Just u:us) = case split us of (us1,us2) -> (u:us1,us2) -- too strict

lazy ~(x:xs) = x:lazy xs

lazyArray n b us = listArray b (lazy us)

add :: Table -> Table
add t@(T {next=next}) =
    bump (t { next=next+1 })
  where
    bump t@(T {code_size=code_size,maxcode=maxcode}) =
      if next==maxcode && code_size<max_code_size
      then t {code_size=code_size+1,maxcode=maxcode*2+1}
      else t

--addtofm m k v = Map.addToFM m k v -- costs center for profiling

dlookup :: Code -> Table -> Maybe Bytes
dlookup c (T {clear=clear,next=next,entries= entries:_}) =
  if c<clear
  then Just (BS [fromIntegral c])
  else if c<next
       then Just (dlookup' c entries)
       else Nothing

dlookup' i t = --trace ("dlookup' "++show i) $
               t!i

dget :: Table -> Code -> Bytes
dget table i =
  case dlookup i table of
    Just y -> y
    Nothing -> error ("GIFdecompress.dget: "++show i++" not in table")

get_next :: Table -> BitStream -> (Code,BitStream)
{- -- Simple version:
get_next (T{code_size}) bs =
  case splitAt code_size bs of
    (w1,bs) -> --trace ("get "++show code_size++" = "++show (int w1)) $
               (int w1,bs)
-}

{- -- More efficient(?) version:
get_next (T{code_size,eoi}) = g code_size 1 0
  where
    g 0 e a bs = (a,bs)
    g n e a (b:bs) = g (n-1) (2*e) (a+e*fromEnum b) bs
    g n _ _ [] = trace ("GIFdecompress.get_next: expected "++show code_size++" bits, but got only "++show (code_size-n))
		 (eoi,[])
-}

-- Even more(?) efficient version:
get_next (T{code_size=code_size,eoi=eoi}) = {-tr.-} btake eoi code_size
  --where tr r@(c,_) = trace (show (code_size,c)) r
