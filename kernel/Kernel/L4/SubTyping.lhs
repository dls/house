\haskellOnly{%

> module Kernel.L4.SubTyping where

}%

%------------------------------------------------------------------------------
Subtyping (a la Liang, Hudak, and Jones for system call requests)

> class Inject a b where
>   inj :: a -> b
> instance Inject () () where
>   inj = id
> instance Inject a (Either a b) where
>   inj = Left
> instance Inject a b => Inject a (Either c b) where
>   inj = Right . inj
