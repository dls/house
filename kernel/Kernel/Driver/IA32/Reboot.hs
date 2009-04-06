module Kernel.Driver.IA32.Reboot where
import Kernel.Driver.CMOS(clearShutdownStatus)
import Kernel.Driver.PS2(resetProcessor)
import H.AdHocMem(H,Ptr,Word32,absolutePtr,poke)

reboot :: H ()
reboot =
   do -- probably should turn off interrupts
      clearShutdownStatus -- tell BIOS this was a normal shutdown
      poke ((absolutePtr 0x472):: Ptr Word32) 0x1234  -- and that we want a quick restart; this seems a dangerous addr to write
      resetProcessor -- ask for it
