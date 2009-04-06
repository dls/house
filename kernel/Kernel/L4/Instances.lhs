
> module Kernel.L4.Instances where

> import Kernel.L4.Syscall
> import Kernel.L4.Req
> import Kernel.UserSpace
> import UProc

> instance Syscall Prio where
>   handle up c (SetPriority p) = return c

Unmap steps:
1.  determine if flush or unmap will be done
2.  perform unmap/flush, get one tree list per fpage in same order as fpage list
3.  map unassoc across each list, concat results
4.  fold orPerms across each list
5.  replace perms of each fpage with the corresponding perms value

> instance Syscall Unmap where
>   handle up c (Unmap r size) = return c

%------------------------------------------------------------------------------
\subsection{ThreadSwitch}

(L4 names, but not with L4 semantics, yet)

The \hs{ThreadSwitch} system call is implemented as follows:
 
> instance Syscall ThreadSwitch where
>   handle up c (ThreadSwitch _ti) = return c{eax=70}

> instance Syscall IPC where
>   handle up c ipc = return c

> instance Syscall ThreadControl where
>   handle up c tc = return c
