-- | Unsafe operations

module H.Unsafe where

import System.IO.Unsafe(unsafePerformIO)
import H.Monad(H,runH)

unsafePerformH :: H a -> a
unsafePerformH = unsafePerformIO . runH
