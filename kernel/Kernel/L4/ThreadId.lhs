
\haskellOnly{%

> module Kernel.L4.ThreadId where

}%

%------------------------------------------------------------------------------

\emph{Threads} are identified by positive integers. The value \hs{NoThread}
is used to initialize variables when no thread id is visible at the time
of initialization.

> data ThreadId
>   = ThreadId Int  -- Positive integer identifying the thread
>   | Nilthread
>     deriving (Eq, Show)

A class the generalizes the notion of "having a thread ID"

> class ThreadIds t where
>   threadIds    :: t -> [ThreadId]
>   usedIn       :: ThreadId -> t -> Bool
>   ti `usedIn` x = ti `elem` threadIds x    -- this should be a property too

> instance ThreadIds ThreadId where
>   threadIds ti  = [ti]
>   usedIn        = (==)

> instance ThreadIds t => ThreadIds [t] where
>   threadIds = concat . map threadIds


