-- | Memory regions for memory-mapped IO and other reserved memory
-- (section 3.4 in the paper)
module H.MemRegion(
       H,
       MemRegion,Offset,Size,createRegion,regionSize,--withTempRegion,
       pokeByteOff,peekByteOff,pokeElemOff,peekElemOff,moveBytes,copyArray,
       fillRegion,pokeArray,
       uncheckedPokeElemOff,
       Storable,SafePtr,safePtr,peek,poke,
       -- * Deprecated
       pokeByteIntOff,peekByteIntOff,pokeElemIntOff,peekElemIntOff
       ) where
import qualified H.AdHocMem as U
import H.AdHocMem(H,Ptr,castPtr,plusPtr,allocaArray)
import Foreign.Storable(Storable,sizeOf)
import Data.Word(Word8,Word32)
import Data.Array.IArray(bounds,(!))
import Data.Ix(rangeSize)
-- For SPECIALIZE pragma:
import Data.Array.Unboxed(UArray)
import Data.Word(Word8,Word16,Word32)

-------------------------- INTERFACE -------------------------------------------

---- Memory regions ----

--type MemRegion
type Offset = Word32
type Size = Word32

createRegion :: Ptr Word8 -> Offset -> MemRegion                 -- unsafe!
regionSize :: MemRegion -> Offset

--withTempRegion :: Size -> (MemRegion->H ()) -> H ()
        -- It's unsafe to return the temporary region.

--- Accessing a memory region, requires dynamic safety checks:
pokeByteOff  :: Storable a => MemRegion -> Offset -> a -> H ()
peekByteOff  :: Storable a => MemRegion -> Offset -> H a
pokeElemOff  :: Storable a => MemRegion -> Offset -> a -> H ()
peekElemOff  :: Storable a => MemRegion -> Offset -> H a

uncheckedPokeElemOff :: Storable a => MemRegion -> Offset -> a -> H () --unsafe!

moveBytes    ::               MemRegion->Offset->MemRegion->Offset->Size-> H ()
copyArray    :: Storable a => MemRegion->Offset->Ptr a            ->Size-> H ()

--- Accessing memory with signed Int offsets, not recommended
{-# DEPRECATED pokeByteIntOff, peekByteIntOff, pokeElemIntOff, peekElemIntOff
    "Use Offset (Word32) instead of Int for memory region offsets" #-}
pokeByteIntOff  :: (Storable a) => MemRegion -> Int -> a -> H ()
peekByteIntOff  :: (Storable a) => MemRegion -> Int -> H a
pokeElemIntOff  :: (Storable a) => MemRegion -> Int -> a -> H ()
peekElemIntOff  :: (Storable a) => MemRegion -> Int -> H a


---- Safe Pointers ----
--type SafePtr a
--- Creating a safe pointer, requires a dynamic safety check
safePtr :: Storable a => MemRegion -> Offset -> H (SafePtr a)

--- Access to memory via safe pointers, free from dynamic checks
poke :: (Storable a) => (SafePtr a) -> a -> H ()
peek :: (Storable a) => (SafePtr a) -> H a

-------------------------- IMPLEMENTATION --------------------------------------

data MemRegion = MR { base:: !(Ptr Word8), size:: !Size }
     deriving Show

newtype SafePtr a = SafePtr (Ptr a)

-- Usafe:
createRegion = MR -- could also check for overlap with known regions

regionSize = size

--withTempRegion size h = allocaArray (w2i size) (\ p -> h (MR p size))

safePtr (MR base size) o =
  case base +. o of
    p -> if o+s<=size
         then return (SafePtr p)
         else fail "Trying to create pointer to object outside region"
      where
	s = elSize (deref p)

	deref :: Ptr a -> a -- just a type checking trick
	deref = undefined

poke (SafePtr p) = U.poke p
peek (SafePtr p) = U.peek p

pokeByteOff mr o x =
  do p <- safePtr mr o
     poke p x

peekByteOff mr o =
  do p <- safePtr mr o
     peek p

pokeElemOff mr o x = pokeByteOff mr (o*elSize x) x
{-
pokeElemOff (MR base size) eo x | o+s<=size = U.pokeByteOff base (w2i o) x
  where --"pokeElemOff outside region"
    s = elSize x
    o = eo*s
-}

uncheckedPokeElemOff (MR base _) o x =
    U.pokeElemOff (castPtr base) (w2i o) x

peekElemOff mr o = peekElemOff' mr o undefined
  where
    peekElemOff'  :: (Storable a) => MemRegion -> Word32 -> a -> H a
    peekElemOff' mr o dummy = peekByteOff mr (o*elSize dummy)

pokeByteIntOff mr o | o>=0 = pokeByteOff mr (i2w o)
peekByteIntOff mr o | o>=0 = peekByteOff mr (i2w o)
pokeElemIntOff mr o | o>=0 = pokeElemOff mr (i2w o)
peekElemIntOff mr o | o>=0 = peekElemOff mr (i2w o)

moveBytes dstr dsto srcr srco n =
  do dst <- safePtr' dstr dsto n
     src <- safePtr' srcr srco n
     U.moveBytes dst src (w2i n)

safePtr' :: MemRegion -> Offset -> Offset -> H (Ptr Word8)
safePtr' (MR base size) o esize =
    if o+esize<=size
    then return (base +. o)
    else fail "Trying to create pointer to object outside region"

copyArray dstr dsto ptr n = copyArray' dstr dsto ptr undefined n
  where
    copyArray' :: Storable a=>MemRegion->Offset->Ptr a->a->Offset->H ()
    copyArray' dstr dsto src dummy n =
      do let el=elSize dummy
             bcnt = n*el
         dst <- safePtr' dstr (dsto*el) bcnt
	 U.moveBytes dst (castPtr src) (w2i bcnt)

fillRegion (MR b s) x =
    sequence_ [U.pokeElemOff (castPtr b) i x|i<-[0..w2i s `div` sizeOf x]]

pokeArray mr o a =
    do dst <- safePtr' mr o (elSize (a!undefined)*cnt)
       U.pokeArray (castPtr dst) a
  where
    cnt = i2w $ rangeSize (bounds a)

type PokeArray d = MemRegion -> Offset -> UArray Int d -> H ()
{-# SPECIALIZE pokeArray :: PokeArray Word8 #-}
{-# SPECIALIZE pokeArray :: PokeArray Word16 #-}
{-# SPECIALIZE pokeArray :: PokeArray Word32 #-}

-- Unsigned versions of plusPtr and sizeOf:
p +. n = plusPtr p (w2i n)
elSize x = i2w (sizeOf x)

--- fromIntegral is such a loooong name...
i2w :: Int->Word32
w2i :: Word32->Int
i2w = fromIntegral
w2i = fromIntegral
