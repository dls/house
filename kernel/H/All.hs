-- For convenience, unites all parts of H in one module.
-- Typically, it is better style to import the individual parts separately.

-- See the imported modules for details of the H interface.

module H.All(module H.All) where

-- The H monad and its operations:
import H.Monad         as H.All
import H.PhysicalMemory as H.All
import H.VirtualMemory as H.All
import H.UserMode      as H.All
import H.IOPorts       as H.All
import H.Interrupts    as H.All
import H.MemRegion     as H.All hiding (createRegion) -- hide unsafe function
import H.Grub	       as H.All hiding (moduleStart,moduleEnd)

-- Features inherited from the IO monad:
import H.Mutable       as H.All
import H.Concurrency   as H.All

-- Things excluded from the main H interface:
--import H.AdHocMem    as H -- Exports unsafe memory operations
--import H.MonadImpl   as H -- Reveals internal representation
--import H.Unsafe      as H -- Unsafe operations inherited from the IO monad

import Control.Monad(liftM)

---------------- PROPERTIES FOLLOW ---------------

{-P:
-- A monadic computations returns a given value:
property Returns x = {| m | m==={m>>return x} |}

-- Two computations are independent in all contexts
property Commute =
  {| f,g | {do x <- f; y <- g; return (x,y)}
            === {do y <- g; x <- f; return (x,y)} |}

-- Two computations are independent immediately following computation m
property PostCommute f g
   = {| m | { do m; x <- f; y <- g; return (x,y) } === {do m; y <- g; x <- f; return (x,y) } |}

property StateGetGet =
  {| get | {do x <- get; y <- get; return x } === { do x <- get ; y <- get; return y } |}

property StateSetSet = 
  {| set | All x, x'. { do set x; set x' } === { do set x' } |}

property StateSetGet set get = All x. { do set x; get } ::: Returns x

-- Normal stateul operations.
property Stateful = {| set, get |
      StateGetGet get /\ StateSetSet set /\ StateSetGet set get |}

-----

property IndependentSetSet = {| set, set' | All x, x'. Commute {set x} {set' x'} |}

property IndependentSetGet = {| set, get |  All x . Commute {set x} {get} |}

property Independent =
  {| set, get, set', get' |
     IndependentSetSet set set' /\ IndependentSetGet set get' /\ IndependentSetGet set' get |}

--------------
-- each use of f generates a different result

property JustGenerative f = All m.
    {do Just x <- f; m; Just y <- f; return (x==y)} ::: Returns {False}

--------------

property ValidVAddr = {| v::VAddr | True {v >= minVAddr && v <= maxVAddr} |}

property OnSamePage = {| v::VAddr,v'::VAddr | {v `div` (fromIntegral pageSize)} === {v' `div` (fromIntegral pageSize)} |}

-- property AlignedVAddr =
--    {| vaddr::VAddr |  True {fromIntegral vaddr `mod` pageSize == 0} |}

-- property PageVAddr = ValidVAddr /\ AlignedVAddr

-- Following m, pa is not mapped by any va in pm
property NotMapped pm pa =
  {| m | All va . va ::: ValidVAddr ==>
	   {do m; isMappedTo pm va pa} ::: Returns {False} |}

-- Following m, pa is not mapped writable by any va in pm
property NotMappedWritable pm pa =
  {| m | All va . va ::: ValidVAddr ==>
	   {do m; isMappedWritableTo pm va pa} ::: Returns {False} |}

-- va is mapped to ppg in pm
isMappedTo pm va (ppg,_) =
    do pme <- getPage pm va
       case pme of
	 Just PageInfo{physPage = ppg'} -> return (ppg == ppg')
	 Nothing -> return False

-- va is mapped writeable to ppg in pm
isMappedWritableTo pm va (ppg,_) =
    do pme <- getPage pm va
       case pme of
	 Just PageInfo{physPage = ppg',writeable=True} -> return (ppg == ppg')
	 _ -> return False


-- Paddrs act like distinct stateful locations.

assert All pa, pa',x . pa =/= pa' ==>
            Stateful {setPAddr pa} {getPAddr pa}
             /\ Independent {setPAddr pa} {getPAddr pa} {setPAddr pa'} {getPAddr pa'}

-- page map entries are independent stateful locations

assert All pm, pm', va, va'. va ::: ValidVAddr /\ va' ::: ValidVAddr /\ --
                 (pm =/= pm' \/ -/ OnSamePage {va} {va'})  ==>
     Stateful {setPage pm va} {getPage pm va}
     /\ Independent {setPage pm va} {getPage pm va} {setPage pm' va'} {getPage pm' va'}


-- page maps are initially zero

assert All va. va ::: ValidVAddr ==>
          {do Just pm <- allocPageMap; getPage pm va} ::: Returns {Nothing}

-- page map entries and physical addresses are independent too

assert All pm, pa, va . va ::: ValidVAddr ==> 
     Independent {setPage pm va} {getPage pm va} {setPAddr pa} {getPAddr pa}

-- IO ports are independent from physical pages and page tables:

assert All p, pa . Independent {outB p} {inB p} {setPAddr pa} {getPAddr pa}

assert All p, pm, va . va ::: ValidVAddr ==> 
     Independent {outB p} {inB p} {setPage pm va} {getPage pm va}

-- execution cannot change the contents of a physical address not mapped writeable

assert 
   All pm, pa, m, c .
   m ::: NotMappedWritable pm pa ==>
   m ::: PostCommute {getPAddr pa} {execContext pm c}

-- changing the contents of an unmapped address cannot affect execution

assert 
   All pm, pa, m, c, x .
    m ::: NotMapped pm pa ==>
    m ::: PostCommute {setPAddr pa x} {execContext pm c}


-- changing one page table has no effect on execution under another page table

assert
   All pm, pm', va, c, x .
   pm =/= pm' ==> Commute {setPage pm' va x} {execContext pm c}


-- executing under one page table has no effect on the contents of any other page table

assert
   All pm, pm', va, c .
   pm =/= pm' ==> Commute {getPage pm' va} {execContext pm c}


-- execution under a page table has no effect on the page mapping and writeable status of any entry in that table

assert All pm, va, c . Commute {getPageField physPage pm va} {execContext pm c}
assert All pm, va, c . Commute {getPageField writeable pm va} {execContext pm c}

getPageField field pm va = liftM (fmap field) (getPage pm va)

-- if execution under a page table changes some physical address, then there must
-- be an entry mapping that address somewhere in the table with its dirty and access bits set
-- here's the best try so far:

property Changed pa pm c 
  = {| m | 
      {do m; x <- getPAddr pa; execContext pm c; y <- getPAddr pa; return (x == y)} 
      ::: Returns {False} |}

property Dirty pa pm c
  =  {| m | Exist va . {do m; execContext pm c; getPage pm va} 
			 ::: Returns {Just PageInfo{ physPage = fst pa,
						     writeable = True, 
						     dirty = True,
						     accessed = True }} |}
assert
  All pa, pm, c, m .
    m ::: Changed pa pm c ==> m ::: Dirty pa pm c

-}
