%- A monad transformer for state: ---------------------------------------------
\subsection{The State Monad Transformer, \hs{ST}}

\haskellOnly{%

> module Monad.State where

> import Monad.MonadT

}%

> newtype ST s m a = ST { unST :: (s -> m (a, s)) }

> infixl `runST`
> runST          :: Monad m => ST s m a -> s -> m (a, s)
> runST (ST c) s  = c s

\haskellOnly{%

> infixl `runSTs`
> runSTs         :: Monad m => ST s m a -> s -> m s
> runSTs (ST c) s = do (a,s') <- c s
>                      return s'

}%

> instance Monad m => Monad (ST s m) where
>   return x   = ST (\s -> return (x, s))
>   ST c >>= f = ST (\s -> do (a, s') <- c s
>                             let ST c' = f a
>                             c' s')

> instance MonadT (ST s) where
>   lift m = ST (\s -> do a <- m
>                         return (a, s))

%- Morphing between state monads: ---------------------------------------------

\haskellOnly{%

> data Embedding full part = Embedding {
>                              inject  :: part -> full -> full,
>                              project :: full -> part
>                            }

> embedST :: Monad m => Embedding full part -> ST part m a -> ST full m a
> embedST emb (ST c) = ST (\full -> do (r, part) <- c (project emb full)
>                                      return (r, inject emb part full))

}%
        
%- A class of state monads: ---------------------------------------------------

> class Monad m => StateMonad s m where
>   get     :: m s
>   set     :: s -> m ()
>   update  :: (s -> s) -> m ()
>   update f = do s <- get
>                 set (f s)

> instance Monad m => StateMonad s (ST s m) where
>   get      = ST (\s -> return (s, s))
>   set s    = ST (\_ -> return ((), s))
>   update f = ST (\s -> return ((), f s))

> instance (MonadT t, Monad (t m), StateMonad s m)
>   => StateMonad s (t m) where
>   get      = lift get
>   set s    = lift (set s)
>   update f = lift (update f)

%------------------------------------------------------------------------------
