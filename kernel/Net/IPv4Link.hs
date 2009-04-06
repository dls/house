module Net.IPv4Link where

-- Routing of IP packets in the simple where there is a single
-- link which has a router.

import Net.Interface as Net
import Net.IPv4

initialize net optRouter link = Interface { rx=rx link, tx=tx }
  where
    tx = maybe txlocal txr optRouter
    txr routerIP ip = Net.tx link $ if sameNet net destIP
	                            then (destIP,ip)
		 	            else (routerIP,ip)
      where destIP = dest ip
    txlocal ip = if sameNet net destIP
		 then Net.tx link (destIP,ip)
		 else return () -- no route, dropping packet
      where destIP = dest ip
