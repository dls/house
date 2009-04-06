-- | Some useful monadic combinators missing from the standard libraries
module Monad.Util where
import Control.Monad(ap,when)

infixl 1 #,#!,<#

-- | Apply a pure function to the result of a monadic computation
f # x = fmap f x

-- | Apply a function returned by a monadic computation to an argument returned
-- by a monadic computation
f <# x = ap f x

-- | Perform two monadic computation and return the result from the second one
x #! y = const # x <# y


-- It is a scandal that monadic composition isn't defined in the libraries...
infixr 1 @@
-- | Kleiski composition
(m1 @@ m2) i = m1 =<< m2 i

-- | Infinite loops
loop m = l where l = m>>l

-- | While loops
whileM cndM bodyM = loop
  where loop = do more <- cndM
		  when more (bodyM >> loop)

-- | Repeat m while it returns True
repeatM m = whileM m (return ())

done :: Monad m => m ()
done = return ()
