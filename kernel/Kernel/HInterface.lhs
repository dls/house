-- THIS SHOULD BE RENAMED TO SOMETHING LIKE "HCLASSES" 
-- AND PROPERTIES SHOULD GO ELSEWHERE

> module Kernel.HInterface where

> import Data.Word(Word8)
> import Kernel.HDatatypes
> import H.IOPorts
> import H.Interrupts(IRQ)
> import Monad.MonadT

! import H.All(Stateful,Independent,ValidVAddr,OnSamePage,NotMappedWriteable,
!              NotMapped,PostCommute,Commute)
! import Kernel.UserSpace

%------------------------------------------------------------------------------
Physical Memory

> class Monad m => HPhysMemMonad m where
>   getPAddr :: PAddr -> m Word8
>   setPAddr :: PAddr -> Word8 -> m ()

> instance (MonadT t, Monad (t m), HPhysMemMonad m) => HPhysMemMonad (t m) where
>   getPAddr pa   = lift (getPAddr pa)
>   setPAddr pa b = lift (setPAddr pa b)

Properties

Paddrs act like distinct stateful locations.

! assert All pa, pa',x . -/ pa === pa' ==>
!             Stateful {setPAddr pa} {getPAddr pa}
!             /\ Independent {setPAddr pa} {getPAddr pa} {setPAddr pa'} {getPAddr pa'}

%------------------------------------------------------------------------------
Page Table Entries

> class Monad m => HPageMonad m where
>   getPage :: PageMap -> VAddr -> m (Maybe PageInfo)
>   setPage :: PageMap -> VAddr -> Maybe PageInfo -> m Bool

> instance (MonadT t, Monad (t m), HPageMonad m) => HPageMonad (t m) where
>   getPage pm va     = lift (getPage pm va)
>   setPage pm va mpi = lift (setPage pm va mpi)

Properties

page map entries are independent stateful locations

! assert All pm, pm', va, va'. va ::: ValidVAddr /\ va' ::: ValidVAddr /\ --
!                  ((-/ (pm === pm')) \/ (-/ (OnSamePage {va} {va'})))  ==>
!      Stateful {setPage pm va} {getPage pm va}
!      /\ Independent {setPage pm va} {getPage pm va} {setPage pm' va'} {getPage pm' va'}

%------------------------------------------------------------------------------
Page Tables

> class Monad m => HPageMapMonad m where
>   allocPageMap :: m (Maybe PageMap)
>   freePageMap  :: PageMap -> m ()

> instance (MonadT t, Monad (t m), HPageMapMonad m) => HPageMapMonad (t m) where
>   allocPageMap   = lift allocPageMap
>   freePageMap pm = lift (freePageMap pm)

Properties

each use of f generates a different result

! property JustGenerative = {| f | All m.
!     {do Just x <- f; m; Just y <- f; return (x==y)}
!    === {do Just _ <- f; m; Just _ <- f; return False} |}

Note: this isn't quite right for our intended use, because
on the RHS, GC might occur between the calls to f, leading
to two alloc calls returning the same thing.

each call to allocPageMap returns a unique PageMap

! assert JustGenerative allocPageMap

page maps are initially zero

! assert All va. va ::: ValidVAddr ==>
!           {do Just pm <- allocPageMap; getPage pm va}
!            === {do Just _ <- allocPageMap; return Nothing} 

does this work properly in the Nothing case? 

%------------------------------------------------------------------------------
Physical + Virtual Memory 

> class    (HPhysMemMonad m, HPageMonad m) => HMemoryMonad m 
> instance (HPhysMemMonad m, HPageMonad m) => HMemoryMonad m

Properties:

page map entries and physical addresses are independent too

! assert All pm, pa, va . va ::: ValidVAddr ==> 
!      Independent {setPage pm va} {getPage pm va} {setPAddr pa} {getPAddr pa}

IO ports are independent from physical pages and page tables:

! assert All p, pa . Independent {outB p} {inB p} {setPAddr pa} {getPAddr pa}

! assert All p, pm, va . va ::: ValidVAddr ==> 
!      Independent {outB p} {inB p} {setPage pm va} {getPage pm va}

%------------------------------------------------------------------------------
Execution

> class Monad m => HExecMonad m where
>   execContext :: PageMap -> Context -> m (Interrupt, Context)

> instance (MonadT t, Monad (t m), HExecMonad m) => HExecMonad (t m) where
>   execContext pm c = lift (execContext pm c)

%------------------------------------------------------------------------------
IO Operations

> class Monad m => HIOMonad m where
>   registerIRQHandler :: IRQ -> H () -> m ()
>   callIRQHandler     :: IRQ -> m ()

> instance (MonadT t, Monad (t m), HIOMonad m) => HIOMonad (t m) where
>   registerIRQHandler irq h = lift (registerIRQHandler irq h)
>   callIRQHandler irq       = lift (callIRQHandler irq)

%------------------------------------------------------------------------------
Memory + Page Tables + Execution

> class (HMemoryMonad m, HPageMapMonad m, HExecMonad m) => HMonad m
> instance (HMemoryMonad m, HPageMapMonad m, HExecMonad m) => HMonad m

Properties

-- execution cannot change the contents of a physical address not mapped writeable

! assert 
!    All pm, pa, m, c .
!    m ::: NotMappedWriteable pm pa ==>
!    m ::: PostCommute {getPAddr pa} {execContext pm c}

-- changing the contents of an unmapped address cannot affect execution

! assert 
!    All pm, pa, m, c, x .
!     m ::: NotMapped pm pa ==>
!     m ::: PostCommute {setPAddr pa x} {execContext pm c}


-- changing one page table has no effect on execution under another page table

! assert
!    All pm, pm', va, c, x .
!    -/ pm === pm' ==> Commute {setPage pm' va x} {execContext pm c}


-- executing under one page table has no effect on the contents of any other page table

! assert
!    All pm, pm', va, c .
!    -/  pm === pm' ==> Commute {getPage pm' va} {execContext pm c}


-- execution under a page table has no effect on the page mapping and writeable status of any entry in that table

! assert
!    All pm, va, c .
!      Commute {do pme <- getPage pm va
!                  case pme of Just PageInfo {physPage = ppg, writeable=w} -> return (Just (ppg,w))
!                              Nothing -> return Nothing}
!              {execContext pm c}

-- if execution under a page table changes some physical address, then there must
-- be an entry mapping that address somewhere in the table with its dirty and access bits set
-- here's the best try so far:

! property Changed pa pm c 
!   = {| m | 
!       {do m; x <- getPAddr pa; execContext pm c; y <- getPAddr pa; return (x == y)} 
!       === {do m; execContext pm c; return False} |}

! property Dirty pa pm c
!   =  {| m | Exist va . {do m; execContext pm c; getPage pm va} 
!      === {do m; execContext pm c; return (Just PageInfo{ physPage = fst pa,
!                                                          writeable = True, 
!                                                          dirty = True,
!                                                          accessed = True })} |}
                                                 
! assert
!  All pa, pm, c, m .
!    m ::: Changed pa pm c ==> m ::: Dirty pa pm c


