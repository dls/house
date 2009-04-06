
A Haskell model for L4

\haskellOnly{%

> module Kernel.L4.Syscall where

> import Kernel.UserSpace
> import UProc

}%

%------------------------------------------------------------------------------
\subsection{Support for an extensible request type}

> class Syscall sc where
>   handle :: (Monad m) => UProc -> Context -> sc -> m Context

> instance Syscall () where
>   handle _ c () = return c

> instance (Syscall a, Syscall b) => Syscall (Either a b) where
>   handle up c (Left x)  = handle up c x
>   handle up c (Right y) = handle up c y
