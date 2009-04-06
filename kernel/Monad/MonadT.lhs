%- Monad Transformers: --------------------------------------------------------
%- Maybe we should use the "official" libraries instead ...

\subsection{Monad Transformers}

\haskellOnly{

> module Monad.MonadT where

}%

> class MonadT t where
>   lift :: Monad m => m a -> t m a


%------------------------------------------------------------------------------
