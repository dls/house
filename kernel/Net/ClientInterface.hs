module Net.ClientInterface where

import Data.Word(Word16)
import qualified Net.IPv4 as IP
import qualified Net.ARP_Protocol as ARP
import qualified Net.UDP_Client as UC
import qualified Net.TCP_Client as TC

data Config = DHCP
	    | Fixed { myIP,routerIP,netmask::IP.Addr }
	      deriving (Show)

fixed myIP routerIP = Fixed myIP routerIP (IP.defaultNetmask myIP)

data Net m
  = Net {
      ping :: IP.Addr -> Word16 -> Word16 -> m (),
      dump :: m ARP.CacheDump,
      udp  :: UC.Interface m,
      tcp  :: TC.Interface m
    }
