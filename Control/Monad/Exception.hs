{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}

module Control.Monad.Exception (
    EM,  evalEM, runEM, runEMParanoid,
    EMT, evalEMT, runEMT, runEMTParanoid,
    MonadZeroException(..),
    module Control.Monad.Exception.Class ) where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Exception.Class
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad.Cont.Class
import Control.Monad.RWS.Class
import Data.Monoid
import Data.Typeable
import Prelude hiding (catch)

type EM l = EMT l Identity


{-
data AnyException

instance Exception e => Throws e AnyException

-- | Run a computation which may fail
evalEM :: EM AnyException a -> Either SomeException a
evalEM (EMT a) = mapLeft wrapException (runIdentity a)
-}
mapLeft :: (a -> b) -> Either a r -> Either b r
mapLeft f (Left x)  = Left (f x)
mapLeft _ (Right x) = Right x

-- | Run a computation explicitly handling exceptions
evalEM :: EM (AnyException l) a -> Either SomeException a
evalEM = runIdentity . evalEMT

-- | Run a safe computation
runEM :: EM NoExceptions a -> a
runEM = runIdentity . runEMT

-- | Run a safe computation checking even unchecked (@UncaughtExceptions@) exceptions
runEMParanoid :: EM ParanoidMode a -> a
runEMParanoid = runIdentity . runEMTParanoid

data MonadZeroException = MonadZeroException deriving (Show, Typeable)
instance Exception MonadZeroException

newtype EMT l m a = EMT {unEMT :: m (Either (WrapException l) a)}

type AnyException = Caught SomeException

-- | Run explicitly handling exceptions
evalEMT :: Monad m => EMT (AnyException l) m a -> m (Either SomeException a)
evalEMT (EMT m) = mapLeft wrapException `liftM` m

runEMT_gen :: Monad m => EMT l m a -> m a
runEMT_gen (EMT m) = liftM f m where
  f (Right x) = x
  f (Left  (WrapException (SomeException e))) = error (show e)

-- | Run a safe computation
runEMT :: Monad m => EMT NoExceptions m a -> m a
runEMT = runEMT_gen

-- | Run a safe computation checking even unchecked (@UncaughtException@) exceptions
runEMTParanoid :: Monad m => EMT ParanoidMode m a -> m a
runEMTParanoid = runEMT_gen

instance Monad m => Functor (EMT l m) where
  fmap f emt = EMT $ do
                 v <- unEMT emt
                 case v of
                   Left  e -> return (Left e)
                   Right x -> return (Right (f x))

instance Monad m => Monad (EMT l m) where
  return = EMT . return . Right
  emt >>= f = EMT $ do
                v <- unEMT emt
                case v of
                  Left e  -> return (Left e)
                  Right x -> unEMT (f x)

instance Monad m => Applicative (EMT l m) where
  pure  = return
  (<*>) = ap

instance (Exception e, Throws e l, Monad m) => MonadThrow e (EMT l m) where
  throw = EMT . return . Left . WrapException . toException
instance (Exception e, Monad m) => MonadCatch e (EMT (Caught e l) m) (EMT l m) where
  catch = catchEMT

catchEMT :: (Exception e, Monad m) => EMT (Caught e l) m a -> (e -> EMT l m a) -> EMT l m a
catchEMT emt h = EMT $ do
                v <- unEMT emt
                case v of
                  Right x -> return (Right x)
                  Left (WrapException e) -> case fromException e of
                               Nothing -> return (Left (WrapException e))
                               Just e' -> unEMT (h e')


instance (Monad m, Throws MonadZeroException l) => MonadPlus (EMT l m) where
  mzero = throw MonadZeroException
  mplus emt1 emt2 = EMT$ do
                     v1 <- unEMT emt1
                     case v1 of
                       Left _  -> unEMT emt2
                       Right _ -> return v1

instance MonadTrans (EMT l) where lift = EMT . liftM Right

instance MonadFix m => MonadFix (EMT l m) where
  mfix f = EMT $ mfix $ \a -> unEMT $ f $ case a of
                                             Right r -> r
                                             _       -> error "empty fix argument"

instance (Throws SomeException l, MonadIO m) => MonadIO (EMT l m) where
  liftIO = lift . liftIO

instance MonadCont m => MonadCont (EMT l m) where
  callCC f = EMT $ callCC $ \c -> unEMT (f (\a -> EMT $ c (Right a)))

instance MonadReader r m => MonadReader r (EMT l m) where
  ask = lift ask
  local f m = EMT (local f (unEMT m))

instance MonadState s m => MonadState s (EMT l m) where
  get = lift get
  put = lift . put

instance (Monoid w, MonadWriter w m) => MonadWriter w (EMT l m) where
  tell   = lift . tell
  listen m = EMT $ do
               (res, w) <- listen (unEMT m)
               return (fmap (\x -> (x,w)) res)
  pass m   = EMT $ pass $ do
               a <- unEMT m
               case a of
                 Left  l     -> return (Left l, id)
                 Right (r,f) -> return (Right r, f)

instance (Monoid w, MonadRWS r w s m) => MonadRWS r w s (EMT l m)