%------------------------------------------------------------------------------
This file demonstrates a simple domain-based operating system running
on top of the hardware model.  This version differs from DomOS2 in
several different ways:

 -  It includes a (privileged) system call for generating new domains

 -  It includes a page fault handler that will assign a fresh physical
    page each time that a page fault is triggered

 -  Privilege is associated with domains rather than individual threads

 -  Because there is no IPC between different domains, we allow the
    same thread ids to be (re)used in each domain

 -  The top-level kernel monad includes components for allocating new
    pagetables and physical pages, and a component for the current
    system.

 -  Domains are deleted when they have no runnable threads left ...

 -  The current thread, and the current domain have reduced scope;
    for example, neither one is in scope in the main scheduler.

%------------------------------------------------------------------------------

> module DomOS4 where

> import Control.Monad(when, guard)
> import Control.Concurrent(yield)

> import Kernel(callIRQHandler)

> import Monad.State
> --import Monad.IO
> import Monad.MonadT

> import Kernel.UserSpace
> import UProc
> import Kernel.L4.SubTyping
> import Kernel.L4.ThreadId
> import Kernel.L4.Req
> --import Kernel.L4.Instances
> import Data.Bits((.&.))
> import Data.Word(Word32)
> import Data.Char(chr)

> type Code = VAddr

> --import USpace
> --import Interface    -- these two instead of HW
> --import Impl

> --main = boot (sample "A")

%------------------------------------------------------------------------------
Operating System State:

> type System   = [Domain]

> data Domain   = Domain {
>                   uproc    :: UProc,
>                   runnable :: [Thread Runnable],
>                   blocked  :: [Thread Blocked]
>                 }

> data Thread s = Thread {
>                   threadId   :: ThreadId,
>                   status     :: s,
>                   priority   :: Int
>                 }

> data Runnable = Runnable { ctxt :: Context }

> data Blocked  = Sending   ThreadId Context
>               | Receiving ThreadId Context

> data Running  = Running

%------------------------------------------------------------------------------
Calculating ThreadIds:

> {-
> class ThreadIds t where
>   threadIds    :: t -> [ThreadId]
>   usedIn       :: ThreadId -> t -> Bool
>   ti `usedIn` x = ti `elem` threadIds x

> instance ThreadIds ThreadId where
>   threadIds ti  = [ti]
>   usedIn        = (==)

> instance ThreadIds t => ThreadIds [t] where
>   threadIds = concat . map threadIds
> -}

> instance ThreadIds (Thread s) where
>   threadIds t   = [threadId t]
>   ti `usedIn` t = ti == threadId t

> instance ThreadIds Domain where
>   threadIds sys = threadIds (runnable sys) ++ threadIds (blocked sys)

%------------------------------------------------------------------------------
Boot/Initialization:

> type Kernel   = ST System H

> {-
> boot  :: Code -> IO ()
> boot c = runH (do u  <- buildUProc
>                   cc <- buildContext u c
>                   scheduler `runSTx` [initDomain u cc])
> -}

> initDomain     :: UProc -> Context -> Domain
> initDomain u c  = Domain { uproc=u, runnable=[t], blocked=[] }
>  where t = Thread {threadId=ThreadId 1, status=Runnable c, priority=100}

%------------------------------------------------------------------------------
Monads and Code:

> class    (StateMonad Domain m, StateMonad System m) => OSMonad m
> instance (StateMonad Domain m, StateMonad System m) => OSMonad m

%------------------------------------------------------------------------------
Thread List Maintenance:

Round robin scheduling of domains:

> insertDomain       :: Domain -> System -> System
> insertDomain d ds   = ds ++ [d]

> selectDomain       :: System -> Maybe (Domain, System)
> selectDomain []     = Nothing
> selectDomain (d:ds) = Just (d, ds)
> --selectDomain ds   = case span (null . runnable) ds of
> --                      (as, b:bs) -> Just (b, bs ++ as)
> --                      _          -> Nothing

%------------------------------------------------------------------------------
The \hs{scheduler} picks the next domain to execute:

> scheduler  :: Kernel Int
> scheduler   = do sys <- get
>                  case selectDomain sys of
>                   Nothing -> do --writeln "no runnable domains"
>                                 return 8
>                   Just (dom, sys')
>                           -> do set sys'
>                                 execDomain dom
>                                 scheduler

\hs{execDomain} picks a thread to execute within a selected domain:

> execDomain     :: Domain -> Kernel ()
> execDomain dom  = case runnable dom of
>                     []     -> lift (freeUProc (uproc dom)) -- this domain is dead!
>                     (t:ts) -> do dom' <- execThread (uproc dom) t
>                                           `runSTs` dom{runnable=ts}
>                                  update (insertDomain dom')

\hs{execThread} invokes a runnable thread and handles its next request.
(Maybe we should be able to separate these ...)

> execThread     :: UProc -> Thread Runnable -> ST Domain Kernel ()
> execThread u t  = do (intr, ctx)
>                        <- lift $
>                           lift $ execContext (pmap u) (ctxt (status t))
>                      handle t{status=Running} ctx intr

Call the following function to schedule an arbitrary runnable thread
for future execution:

> schedule    :: (StateMonad Domain m) => Thread s -> Context -> m ()
> schedule t c = update (\sys -> sys{runnable = insert t' (runnable sys)})
>                where t' = t{status = Runnable c}

> insert        :: Thread s -> [Thread s] -> [Thread s]
> insert t ts    = hi ++ [t] ++ lo
>  where (hi, lo) = span (\t' -> priority t' >= p) ts
>        p        = priority t

The \hs{reschedule} operation arranges for the currently running thread
to be rescheduled to execute a specified piece of code:

> --reschedule    :: (OSMonad m) => Thread Running -> Context -> m ()
> --reschedule t c = start t{status=Runnable c}

Conversely, the \hs{block} operation blocks further execution of
the current thread to await some future IPC operation:

> block      :: (OSMonad m) => Thread Running -> Blocked -> m ()
> block t b   = update (\sys -> sys{blocked = t' : blocked sys})
>               where t' = t{status = b}

%------------------------------------------------------------------------------
System Call Handling:

> class Syscall sc m where
>   handle  :: Thread Running -> Context -> sc -> m ()

> instance (Syscall a m, Syscall b m) => Syscall (Either a b) m where
>   handle t ctx (Left x)  = handle t ctx x
>   handle t ctx (Right y) = handle t ctx y

> instance (Monad m, Syscall sc m) => Syscall (Maybe sc) m where
>   handle t ctx Nothing   = return ()
>   handle t ctx (Just sc) = handle t ctx sc

> instance Syscall Interrupt (ST Domain Kernel) where
>   handle t ctx interrupt
>    = case interrupt of
>        I_ProgrammedException(0x80) -> -- system call
>          let callnum = eax ctx
>              arg = ebx ctx
>   -- putStrLn("Sys call " ++ (show callnum) ++ " " ++ (show arg))
>          in case callnum of
>               0 -> do dom <- get
>                       lift $
>                        lift $ myPutStr (uproc dom)
>                        ("Success with " ++ show (fromIntegral (arg .&. 0xff)))
>               _ -> do dom <- get
>                       result <- lift $ lift $ doSyscall (uproc dom) callnum arg
>                       schedule t ctx{eax=result}
>
>        I_ProgrammedException i -> -- other system call
>          -- do dom <- get
>          --    lift $ lift $ myPutStr (uproc dom)
>          --       ("Prog Exc: " ++ show (makeReq i ctx) ++ "\n")
>             handle t ctx (makeReq i ctx)
>
>        I_PageFault _ faultAddr -> 
>          do dom <- get
>             fixOK <- lift $ lift $ fixPage (uproc dom) faultAddr 
>             if fixOK 
>               then schedule t ctx
>               else return ()
>                    --return ("Fatal page fault at " ++ (showHex faultAddr ""))
>
>        I_ExternalInterrupt 0x00 -> -- timeslice exhausted
>          do lift $ lift $ yield
>             schedule t ctx
>
>        I_ExternalInterrupt irq -> -- other IO interrupts
>          do lift $ lift $ callIRQHandler irq
>             lift $ lift $ yield -- optional
>             schedule t ctx
>
>        _ -> -- any other code means we're done, like it or not
>           return ()
>           -- return ("Unexpected Interrupt: " ++ (show interrupt))

The original House System Calls are supported here ...

> doSyscall :: UProc -> Word32 -> Word32 -> IO Word32
> doSyscall uproc 1 char
>            = do myPutStr uproc [chr (fromIntegral (char .&. 0xff))]
>                 return 0
> doSyscall uproc 2 incr
>            = do mbrk <- extendBrk uproc incr
>                 case mbrk of
>                   Nothing         -> return (-1)
>                   Just currEndBrk -> return currEndBrk
> doSyscall uproc _ _
>            = return (-1)	  

> {-
> syscall    :: Inject sc Req => sc -> Code -> Code
> syscall s c = return (Res (inj s) c)
> -}

%------------------------------------------------------------------------------

Quick Hack: Modified semantics for threadSwitch so that a zero argument
causes a yield, while a nonzero argument terminates the thread ...

> instance (StateMonad Domain m) => Syscall ThreadSwitch m where
>   handle t ctx (ThreadSwitch n)
>    = when (n==Nilthread) (schedule t ctx{eax=0})

Quick Hack: we're not using ThreadControl right now (using Fork instead
as a simpler alternative) so the following code is not used (and doesn't
attempt to implement L4 semantics either ...)

> instance (StateMonad Domain m) => Syscall ThreadControl m where
>   handle t ctx req = schedule t ctx{eax=8}

> instance Syscall Fork (ST Domain Kernel) where -- ugly constraint
>   handle t ctx (Fork tid n)
>     = do dom <- get
>          when (not (tid==Nilthread || tid `usedIn` t  || tid `usedIn` dom))
>           $ do mctx <- lift $ lift $ buildContext (uproc dom) n
>                case mctx of
>                  Nothing   -> return ()
>                  Just nctx -> schedule t{threadId=tid} nctx
>          schedule t ctx

> instance (StateMonad Domain m) => Syscall Unmap m where
>   handle t ctx req = schedule t ctx{eax=8}

> {- See instance for IPC below ...
> instance (StateMonad Domain m) => Syscall IPC m where
>   handle t ctx req = schedule t ctx{eax=8}
> -}

> instance (StateMonad Domain m) => Syscall Prio m where
>   handle t ctx (SetPriority p) = schedule t{priority=p} ctx{eax=0}
>   handle t ctx GetPriority     = schedule t ctx{eax=fromIntegral (priority t)}

> instance (Monad m) => Syscall () m where  -- causes a thread to terminate
>   handle t ctx req = return ()

%------------------------------------------------------------------------------
Domain Operations:

> {-
> instance (OSMonad m, StateMonad PageTable m) => Syscall DomOps m where
>   handle t c (ForkDomain n)
>     = do dom <- get
>          when (privileged (uproc dom))
>           $ do u  <- buildUProc
>                nc <- buildContext u n
>                update (\sys -> sys ++ [ initDomain u nc ])
>          schedule t c

> forkDomain   :: Code -> Code -> Code
> forkDomain c  = syscall (ForkDomain c)
> -}

%------------------------------------------------------------------------------
Basic system calls:

> {-
> instance (OSMonad m) => Syscall Basic m where
>   handle t c Yield
>     = schedule t c
>   handle t c (Fork tid n)
>     = do dom <- get
>          when (not (tid `usedIn` t  || tid `usedIn` dom))
>           $ do nc <- buildContext (uproc dom) n
>                schedule t{threadId=tid} nc
>          schedule t c
>
>   handle t c (SetPriority p)
>     = schedule t{priority=p} c
> {-
>   handle (GetPriority ck)
>     = do (t::Thread Running) <- get
>          reschedule (ck (priority t))
> -}

> yield          :: Code -> Code
> yield           = syscall Yield

> fork           :: ThreadId -> Code -> Code -> Code
> fork tid c      = syscall (Fork tid c)

> setPriority    :: Int -> Code -> Code
> setPriority p   = syscall (SetPriority p)

> --getPriority  :: (Int -> Code) -> Code
> --getPriority f = syscall (GetPriority f)
> -}

%------------------------------------------------------------------------------
Debugging:

> {-
> instance (OSMonad m, IOMonad m) => Syscall Debug m where
>   handle t c (Debug msg) = do writeln msg
>                               schedule t c

> debug        :: String -> String -> Code -> Code
> debug s msg   = syscall (Debug (s ++ ":  " ++ msg))
> -}

%------------------------------------------------------------------------------
IPC:

We're not actually using timeouts in this code ...

> instance {-OSMonad m =>-} Syscall IPC (ST Domain Kernel) where
>   handle t c (Send dst time)
>    = do dom <- get
>         case findFirst (rcvr t) (blocked dom) of
>          Nothing        -> block t (Sending dst c)
>          Just ((bt,bc), bts)
>                         -> do schedule bt bc
>                               schedule t c
>                               copyMsg t c bc
>                               setBlocked bts
>      where -- is bt a valid receiver for this send call?
>            rcvr t bt = do guard (threadId bt == dst)
>                           case status bt of
>                             Receiving mid bc | mid `matches` threadId t
>                               -> return (bt, bc)
>                             _ -> Nothing
>
>   handle t c (Recv msrc time)
>    = do dom <- get
>         case findFirst (sndr t) (blocked dom) of
>           Nothing        -> block t (Receiving msrc c)
>           Just ((bt,bc), bts)
>                          -> do schedule bt bc
>                                schedule t c
>                                copyMsg bt bc c
>                                setBlocked bts
>      where -- is bt a valid sender for this receive call?
>            sndr t bt = do guard (msrc `matches` threadId bt)
>                           case status bt of
>                             Sending src c | src == threadId t
>                               -> return (bt, c)
>                             _ -> Nothing

> copyMsg          :: Thread s -> Context -> Context -> ST Domain Kernel ()
> copyMsg t src dst = lift $ lift $ xferMsg tid src dst
>  where tid = case threadId t of Nilthread  -> 0
>                                 ThreadId n -> n

> setBlocked    :: StateMonad Domain m => [Thread Blocked] -> m ()
> setBlocked bts = update (\sys -> sys{blocked = bts})

> findFirst         :: (a -> Maybe b) -> [a] -> Maybe (b, [a])
> findFirst p []     = Nothing
> findFirst p (x:xs) = case p x of
>                        Just y  -> return (y, xs)
>                        Nothing -> do (y, xs') <- findFirst p xs
>                                      return (y, x:xs')

> matches              :: ThreadId -> ThreadId -> Bool
> matches Nilthread tid = True
> matches tid'      tid = tid'==tid

> {-
> send          :: ThreadId -> Timeout -> Code -> Code
> send tid t     = syscall (Send tid t)

> recv          :: Maybe ThreadId -> Timeout -> Code -> Code
> recv mtid t    = syscall (Recv mtid t)
> -}

%------------------------------------------------------------------------------
Stop:

> {-
> instance Monad m => Syscall Stop m where
>   handle t c Stop = return ()

> instance Inject Stop Stop where
>   inj = id

> stop :: Code
> stop  = syscall Stop halt
> -}

%------------------------------------------------------------------------------

> {-
> instance (HaltMonad m, IOMonad m) => Syscall Interrupt m where
>   handle t c Interrupt = do writeln "Interrupt!"
>                             halt
> -}

%------------------------------------------------------------------------------

> {-
> instance (OSMonad m, PageTableMonad m, StateMonad PhysIdx m)
>           => Syscall PageFault m where
>   handle t c (PageFault va p)
>     = do dom <- get
>          fixPage (uproc dom) (fst va)
>          schedule t c
> -}

%------------------------------------------------------------------------------
Sample Program:

> {-
> multi        = forkDomain (sample "A")
>              $ forkDomain (sample "B")
> --           $ forkDomain (counter "B" 0)
> --           $ forkDomain (idle "C")
>              $ stop

> sample      :: String -> Code
> sample s     = debug s "Demo starting"
>              $ fork 1 (sender s 0)
> --           $ fork 2 (counter s 0)
> --           $ setPriority 50
>              $ receiver s 0

> sender      :: String -> Int -> Code
> sender s n     = updateLocal (\l -> l{msg = n})
>              $ send 0 Infinite
>              $ debug s ("Sender: I just sent " ++ show n)
>              $ if n < 10 then sender s (n+1) else stop

> receiver    :: String -> Int -> Code
> receiver s t = debug s ("Receiver: my total is " ++ show t)
> --           $ setPriority (5*t)
>              $ recv (Just 1) Infinite
>              $ yield -- for demo purposes, not actually necessary!
>              $ getLocal (\l ->
>                 receiver s (t + msg l))

> counter     :: String -> Int -> Code
> counter s n  = debug s ("Counter: I've counted to " ++ show n)
>              $ yield
>              $ if n < 10 then counter s (n+1) else stop
>           -- $ counter (n+1)

> idle        :: String -> Code
> idle s       = debug s ("Idle: yielding")
>              $ yield
>              $ idle s

> -}

%------------------------------------------------------------------------------
