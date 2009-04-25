module Control.Monad.Exception (
    module Control.Monad.Exception,
    module Control.Monad.Exception.Class ) where

import Control.Monad.Exception.Class

newtype EM l a = EM {runEM::Either SomeException a}

instance Monad (EM l) where
  return = EM . Right
  EM (Right x) >>= f = f x
  EM (Left e) >>= _ = EM (Left e)

instance (Exception e, Throws e l) => MonadThrow e (EM l) where
  throw = EM . Left . toException
instance Exception e => MonadCatch e (EM (Caught e l)) (EM l) where
  catch (EM(Right x)) h = EM (Right x)
  catch (EM(Left  e)) h = case fromException e of
                            Nothing -> EM (Left e)
                            Just e' -> h e'
