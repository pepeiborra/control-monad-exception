module Control.Monad.Exception.Instances where

import Control.Monad.Exception.Throws
import Data.Typeable
import Safe.Failure

-- Encoding the exception hierarchy of safe-failure

instance Throws TailFailure (Caught SafeException l)
instance Throws HeadFailure (Caught SafeException l)
instance Throws InitFailure (Caught SafeException l)
instance Throws LastFailure (Caught SafeException l)
instance Throws MinimumFailure (Caught SafeException l)
instance Throws MaximumFailure (Caught SafeException l)
instance Throws Foldr1Failure (Caught SafeException l)
instance Throws Foldl1Failure (Caught SafeException l)
instance Throws FromJustFailure (Caught SafeException l)
instance Throws IndexFailure (Caught SafeException l)
instance Throws ReadFailure (Caught SafeException l)
instance (Typeable a, Show a) => Throws (LookupFailure a) (Caught SafeException l)


