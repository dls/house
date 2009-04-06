{-| Virtual Memory (section 3.2 in the paper)

The virtual memory facilities in the 'H' monad support writing and
reading of /page maps/, which define the translation of /virtual
addresses/ to physical addresses, with associated access permissions and history.  
-}
module H.VirtualMemory(VAddr,minVAddr,maxVAddr,
		       PageMap,allocPageMap,freePageMap,
		       PageInfo(..),setPage,getPage,
		       toPageMap, fromPageMap -- not for public use       
		      ) where
import Data.Word(Word8,Word32)
-- import Kernel.Debug(putStrLn)
import Control.Monad
import H.Monad(H,liftIO)
import H.PhysicalMemory(PhysPage,toPhysPage,fromPhysPage)
import qualified H.Pages as P
import H.AdHocMem
import H.Utils
import H.Mutable(HUArray,newArray,readArray,writeArray)
import H.Unsafe(unsafePerformH)
import Data.Bits

------------------------------- INTERFACE --------------------------------------

-- | @VAddr@ is a concrete type representing virtual addresses.
type VAddr      = Word32

minVAddr, maxVAddr :: VAddr 
minVAddr = 0x10000000
maxVAddr = 0xffffffff


-- abstract type PageMap  -- Show,Eq,Ord(!)

{-| New page maps, represented by the abstract type @PageMap@, are
obtained using @llocPageMap@. The number of available page maps may be limited;
@allocPageMap@ returns  returns @Nothing@ if no more maps are available. -}
allocPageMap :: H(Maybe PageMap)

{-# DEPRECATED freePageMap "freePageMap does nothing" #-}
freePageMap :: PageMap -> H ()


{-|
Page map entries are indexed by valid, aligned, virtual addresses.  The 
entry for an unmapped page contains {\tt Nothing}; the entry for a 
mapped page contains a value, @Just p@, where @p@ is 
of type @PageInfo@.
-}
data PageInfo
  = PageInfo {
      physPage :: PhysPage,
      writable :: Bool,
   -- ^ indicates whether the user process has write access to the page
      dirty :: Bool, 
   -- ^ indicates that the page has been written
      accessed :: Bool
   -- ^ indicates that it has been read or written
    }
    deriving (Eq,Show)

setPage :: PageMap -> VAddr -> Maybe PageInfo -> H Bool
getPage :: PageMap -> VAddr -> H(Maybe PageInfo)


---------- PRIVATE IMPLEMENTATION FOLLOWS -----------------------


type PTE        = Word32
type PTable     = P.Page PTE

type PDE        = Word32
type PDir       = P.Page PDE
data PageMap    = PageMap {fromPageMap::PDir}
  deriving (Show, Eq, Ord)

toPageMap = PageMap

-- bits in PTE's (and PDE's)
bPresent = 0
bWritable = 1
bUser = 2
bAccessed = 5
bDirty = 6
mAddress = 0xfffff000

pteToPageInfo :: PTE -> Maybe PageInfo
pteToPageInfo pte | testBit' bPresent pte =
	              Just (PageInfo {physPage = toPhysPage(ptrFromWord32 (pte .&. mAddress)),
	         	              writable = testBit' bWritable pte,
	                              dirty = testBit' bDirty pte ,
		                      accessed = testBit' bAccessed pte })
                  | otherwise = Nothing

pageInfoToPte :: PageInfo -> PTE
pageInfoToPte pinfo = 
   condBit (writable pinfo) bWritable $
     condBit (dirty pinfo) bDirty $
       condBit (accessed pinfo) bAccessed $
         setBit' bUser $
           setBit' bPresent $
             (ptrToWord32 (fromPhysPage (physPage pinfo))) .&. mAddress

-- Note: these work fine on unaligned VAddrs.
pdOffset :: VAddr -> Int
pdOffset vaddr = fromIntegral ((vaddr `shiftR` 22) .&. 0x3ff)

ptOffset :: VAddr -> Int
ptOffset vaddr =  fromIntegral ((vaddr `shiftR` 12) .&. 0x3ff)

pdeToPTable :: PDE -> Maybe PTable
pdeToPTable pde | testBit' bPresent pde = Just (ptrFromWord32 (pde .&. mAddress))
                | otherwise = Nothing

pTableToPde :: PTable -> PDE
pTableToPde ptable = setBit' bUser $
		       setBit' bWritable $
			 setBit' bPresent $
			   ((ptrToWord32 ptable) .&. mAddress)

pageEntries :: Int
pageEntries = 1024 -- word32s

validVAddr :: VAddr -> Bool
validVAddr vaddr = vaddr >= minVAddr  && vaddr <= maxVAddr


-- check if a ptable is empty (a bit expensive)
isEmptyPtable :: PTable -> H Bool
isEmptyPtable ptable =
   do infos <- sequence [ do {pte <- peekElemOff ptable pto; return (pteToPageInfo pte)} | pto <- [0..pageEntries-1]]
      return $ all (==Nothing) infos

-- invalidate a page in the current PDir
invalidate :: PDir -> VAddr -> H ()
invalidate pdir vaddr =
   do pdir0 <- currentPDir
      if pdir == pdir0 
       then invalidatePage vaddr
       else return ()

getPage (PageMap pdir) vaddr | validVAddr vaddr
    = do pde <- peekElemOff pdir (pdOffset vaddr)	
         case pdeToPTable pde of
           Nothing -> return Nothing 
           Just ptable -> 
             do pte <- peekElemOff ptable (ptOffset vaddr)
                return (pteToPageInfo pte)

setPage (PageMap pdir) vaddr Nothing | validVAddr vaddr
    = do let pdo = pdOffset vaddr
         pde <- peekElemOff pdir pdo
         case pdeToPTable pde of
           Nothing     -> return True  -- nothing to do: page wasn't mapped anyway
           Just ptable ->
            let pto = ptOffset vaddr
            in do pte <- peekElemOff ptable pto
                  case pteToPageInfo pte of
                    Nothing -> return ()  -- page wasn't mapped anyway
                    Just _  -> do pokeElemOff ptable pto 0
                                  invalidate pdir vaddr
				  empty <- isEmptyPtable ptable  
				  if empty 
                                    then  -- ptable is now empty, so unmap and free it
                                      do pokeElemOff pdir pdo 0
                                         P.freePage ptable
                                    else 
                                      return ()
                  return True
setPage (PageMap pdir) vaddr (Just pinfo) | validVAddr vaddr 
      = let pdo = pdOffset vaddr
        in do pde <- peekElemOff pdir pdo
              case pdeToPTable pde of
                Nothing -> 
                    do mptable <- P.allocPage 
                       case mptable of
                         Nothing     -> return False -- insufficient paging pages
                         Just ptable -> do P.zeroPage ptable
                                           pokeElemOff pdir pdo (pTableToPde ptable)
                                           -- presumably there's nothing to invalidate in this case
                                           setPte ptable
                Just ptable -> setPte ptable 
        where setPte ptable =
                 let pto = ptOffset vaddr
                 in do pokeElemOff ptable pto (pageInfoToPte pinfo) 
                       invalidate pdir vaddr
                       return True

allocPageMap 
    = do mpdir <- P.allocPage
	 case mpdir of
	   Nothing   -> return Nothing
           Just pdir -> do P.zeroPage pdir
                           initPDir pdir
			   let pm = PageMap pdir
			   -- putStrLn("allocate PageMap" ++ show pm)
			   P.registerPage pdir pm freePDir
			   return (Just pm)


freePageMap pmap = return ()  -- nop; underlying Pages are freed when corresponding registered PageMap is discovered dead

freePDir :: PDir -> H()
freePDir pdir
    = do pdir' <- currentPDir 
	 if pdir == pdir' 
           then -- just paranoia; this should never happen
	     error "attempt to free current PDir"
           else
             do mapM_ freeOne [0..pageEntries-1]
 	        P.freePage pdir
      where freeOne pdo = 
              do pde <- peekElemOff pdir pdo 
                 case pdeToPTable pde of
                   Just ptable | P.validPage ptable -> P.freePage ptable  
                   _ -> return ()  -- not mapped or mapped to a system page

foreign import ccall unsafe "userspace.h init_page_dir" initPDirIO :: PDir -> IO ()
initPDir = liftIO . initPDirIO

foreign import ccall unsafe "userspace.h current_pdir" currentPDirIO :: IO PDir

foreign import ccall unsafe "userspace.h invalidate_page" invalidatePageIO :: VAddr -> IO ()

currentPDir = liftIO currentPDirIO
invalidatePage = liftIO . invalidatePageIO


