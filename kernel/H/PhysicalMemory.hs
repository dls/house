-- | Physical Pages and Addresses (section 3.1 in the paper)
module H.PhysicalMemory(PAddr, PhysPage, POffset, 
       		        pageSize,getPAddr,setPAddr,
		        allocPhysPage,freePhysPage,
			toPhysPage,fromPhysPage -- not for public use 
       		       ) where

import Data.Word(Word8,Word32)
import Util.Word12
-- import Kernel.Debug(putStrLn)
import H.Monad(H)
import H.AdHocMem(peek,poke,Ptr,plusPtr)
import qualified H.Pages as P(Page,pageSize,allocPage,freePage,registerPage)
import H.Utils
import Data.Word(Word8,Word32)

------------------------------- INTERFACE --------------------------------------

{-|
The type @PAddr@ represents byte addresses in the machine's raw
physical memory. It is composed from an abstract type @PhysPage@,
representing a physical page, and a numeric @POffset@, representing the offset
of a byte within that page.
-}
type PAddr = (PhysPage,POffset)

-- abstract type PhysPage  -- Eq,Show

{-|
Type @Word12@ behaves analogously to the standard unsigned integer types
(e.g. @Word8@); thus, arithemetic operations
are performed modulo @pageSize@.
-}
type POffset = Word12

pageSize :: Int

-- | The contents of individual addresses can be read using @getPAddr@.

getPAddr :: PAddr -> H Word8

-- | The contents of individual addresses can be written using @setPAddr@.
setPAddr :: PAddr -> Word8 -> H ()

{-| New physical pages are obtained using @allocPhysPage@,
which returns @Nothing@ if no more pages are available.
Each allocated page is distinct. -}
allocPhysPage :: H (Maybe PhysPage)
freePhysPage :: PhysPage -> H () -- deprecated

------------ PRIVATE IMPLEMENTATION FOLLOWS ------------------------------------

{-| @PhysPage@s correspond to physical pages that are available
for accesses by user-mode processes, which form a subset of the raw
physical memory installed in the machine. -}
data PhysPage = PhysPage {fromPhysPage::P.Page Word8}
     deriving (Eq,Show)

toPhysPage = PhysPage

pageSize = P.pageSize

getPAddr (PhysPage a,offset) = peek (a `plusPtr` (fromIntegral offset))
setPAddr (PhysPage a,offset) = poke (a `plusPtr` (fromIntegral offset))

allocPhysPage = 
  do mp <- P.allocPage
     case mp of
       Just p -> 
         do let phys = PhysPage p
            -- putStrLn("allocate PhysPage " ++ show phys)
            P.registerPage p phys P.freePage
            return $ Just phys
       Nothing -> return Nothing     

freePhysPage p = return ()  -- nop; Page is freed when corresponding registered PhysPage is discovered dead
