{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Exception (
    module Control.Monad.Exception,
    module Control.Monad.Exception.Class ) where

import Control.Monad.Exception.Class
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad.Cont.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Writer.Class
import Control.Monad.RWS.Class
import Data.Monoid
import Data.Typeable
import Prelude hiding (catch)

newtype EM l a = EM {runEM::Either SomeException a}

instance Functor (EM l) where
    fmap f (EM (Left e))  = EM (Left e)
    fmap f (EM (Right x)) = EM (Right (f x))

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

data MonadZero = MonadZero deriving (Show, Typeable)
instance Exception MonadZero


-- Requires undecidable instances
instance Throws MonadZero l => MonadPlus (EM l) where
  mzero                    = throw MonadZero
  mplus (EM (Left _))   p2 = p2
  mplus p1@(EM Right{}) p2 = p1

newtype EMT l m a = EMT {runEMT :: m (Either SomeException a)}

instance Monad m => Functor (EMT l m) where
  fmap f emt = EMT $ do
                 v <- runEMT emt
                 case v of
                   Left  e -> return (Left e)
                   Right x -> return (Right (f x))

instance Monad m => Monad (EMT l m) where
  return = EMT . return . Right
  emt >>= f = EMT $ do
                v <- runEMT emt
                case v of
                  Left e  -> return (Left e)
                  Right x -> runEMT (f x)

instance (Exception e, Throws e l, Monad m) => MonadThrow e (EMT l m) where
  throw = EMT . return . Left . toException
instance (Exception e, Monad m) => MonadCatch e (EMT (Caught e l) m) (EMT l m) where
  catch emt h = EMT $ do
                v <- runEMT emt
                case v of
                  Right x -> return (Right x)
                  Left  e -> case fromException e of
                               Nothing -> return (Left e)
                               Just e' -> runEMT (h e')


instance (Monad m, Throws MonadZero l) => MonadPlus (EMT l m) where
  mzero = throw MonadZero
  mplus emt1 emt2 = EMT$ do
                     v1 <- runEMT emt1
                     case v1 of
                       Left _  -> runEMT emt2
                       Right _ -> return v1

instance MonadTrans (EMT l) where lift = EMT . liftM Right

instance MonadFix m => MonadFix (EMT l m) where
  mfix f = EMT $ mfix $ \a -> runEMT $ f $ case a of
                                             Right r -> r
                                             _       -> error "empty fix argument"

instance MonadIO m => MonadIO (EMT l m) where
  liftIO = lift . liftIO

instance MonadCont m => MonadCont (EMT l m) where
  callCC f = EMT $ callCC $ \c -> runEMT (f (\a -> EMT $ c (Right a)))

instance MonadReader r m => MonadReader r (EMT l m) where
  ask = lift ask
  local f m = EMT (local f (runEMT m))

instance MonadState s m => MonadState s (EMT l m) where
  get = lift get
  put = lift . put

instance (Monoid w, MonadWriter w m) => MonadWriter w (EMT l m) where
  tell   = lift . tell
  listen m = EMT $ do
               (res, w) <- listen (runEMT m)
               return (fmap (\x -> (x,w)) res)
  pass m   = EMT $ pass $ do
               a <- runEMT m
               case a of
                 Left  l     -> return (Left l, id)
                 Right (r,f) -> return (Right r, f)

instance (Monoid w, MonadRWS r w s m) => MonadRWS r w s (EMT l m)