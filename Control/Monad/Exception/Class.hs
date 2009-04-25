module Control.Monad.Exception.Class (
       module Control.Monad,
       module Control.Monad.Exception.Class,
       Exception(..), SomeException) where

import Control.Monad
import Control.Exception (Exception(..), SomeException)
import qualified Control.Exception

class Exception e => Throws e s

class (Monad m, Exception e) => MonadThrow e m where
    throw :: e -> m a

instance Exception e => MonadThrow e IO where
   throw = Control.Exception.throw



class (Exception e, Monad m, Monad m') => MonadCatch e m m' | e m -> m', e m' -> m where
   catch :: m a -> (e -> m' a) -> m' a

instance Exception e => MonadCatch e IO IO where
   catch = Control.Exception.catch

data Caught e l

instance Exception e => Throws e (Caught e l)
instance Throws e l => Throws e (Caught e1 l)
instance Exception e => Throws e (Caught SomeException l)
