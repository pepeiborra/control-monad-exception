{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverlappingInstances #-}
module Control.Monad.Exception.Base where

import Control.Applicative
import Control.Arrow
import Control.Monad.Base
import Control.Monad.Exception.Catch
import Control.Monad.Loc
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.IO.Class
import Control.Failure
import Control.Monad.Fix
import Data.Typeable
import Data.Functor.Identity

type CallTrace = [String]

-- | A Monad Transformer for explicitly typed checked exceptions.
newtype EMT l m a = EMT {unEMT :: m (Either (CallTrace, CheckedException l) a)}

-- | Run a computation explicitly handling exceptions
tryEMT :: Monad m => EMT AnyException m a -> m (Either SomeException a)
tryEMT (EMT m) = mapLeft (checkedException.snd) `liftM` m

tryEMTWithLoc :: Monad m => EMT AnyException m a -> m (Either (CallTrace, SomeException) a)
tryEMTWithLoc = liftM (mapLeft (second checkedException)) . unEMT

runEMTGen :: forall l m a . Monad m => EMT l m a -> m a
runEMTGen (EMT m) = m >>= \x ->
                     case x of
                       Right x -> return x
                       Left (loc,e) -> error (showExceptionWithTrace loc (checkedException e))

data AnyException
data NoExceptions
data ParanoidMode

instance Exception e => Throws e AnyException

-- | Run a safe computation
runEMT :: Monad m => EMT NoExceptions m a -> m a
runEMT = runEMT_gen

-- | Run a safe computation checking even unchecked ('UncaughtException') exceptions
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

  fail s = EMT $ return $ Left ([], CheckedException $ toException $ FailException s)

  emt >>= f = EMT $ do
                v <- unEMT emt
                case v of
                  Left e  -> return (Left e)
                  Right x -> unEMT (f x)

instance Monad m => Applicative (EMT l m) where
  pure  = return
  (<*>) = ap

instance (Exception e, Throws e l, Monad m) => Failure e (EMT l m) where
  failure = throw

#if !MIN_VERSION_failure(0,2,0)
instance (Exception e, Throws e l, Failure e m, Monad m) => WrapFailure e (EMT l m) where
  wrapFailure mkE m
      = EMT $ do
          v <- unEMT m
          case v of
            Right _ -> return v
            Left (loc, CheckedException (SomeException e))
                    -> return $ Left (loc, CheckedException $ toException $ mkE e)
#endif

instance MonadTrans (EMT l) where
  lift = EMT . liftM Right


instance MonadBase b m => MonadBase b (EMT l m) where
    liftBase = liftBaseDefault

instance MonadBaseControl b m => MonadBaseControl b (EMT l m) where
     newtype StM (EMT l m) a = StmEMT {unStmEMT :: ComposeSt (EMT l) m a}
     liftBaseWith = defaultLiftBaseWith StmEMT
     restoreM     = defaultRestoreM   unStmEMT

instance MonadTransControl (EMT l) where
     newtype StT (EMT l) a = StEMT {unStEMT :: Either (CallTrace, CheckedException l) a}
     liftWith f = EMT $ liftM return $ f $ liftM StEMT . unEMT
     restoreT       = EMT . liftM unStEMT

instance Monad m => MonadLoc (EMT l m) where
    withLoc loc (EMT emt) = EMT $ do
                     current <- withLoc loc emt
                     case current of
                       (Left (tr, a)) -> return (Left (loc:tr, a))
                       _              -> return current

instance MonadFix m => MonadFix (EMT l m) where
  mfix f = EMT $ mfix $ \a -> unEMT $ f $ case a of
                                             Right r -> r
                                             _       -> error "empty fix argument"

-- | The throw primitive
throw :: (Exception e, Throws e l, Monad m) => e -> EMT l m a
throw = EMT . return . (\e -> Left ([],e)) . CheckedException . toException

-- | Rethrow an exception keeping the call trace
rethrow :: (Throws e l, Monad m) => CallTrace -> e -> EMT l m a
rethrow callt = EMT . return . (\e -> Left (callt,e)) . CheckedException . toException

showExceptionWithTrace :: Exception e => [String] -> e -> String
showExceptionWithTrace [] e = show e
showExceptionWithTrace trace e = unlines ( show e
                                         : [ " in " ++ show loc | loc <- reverse trace])

-- | UncaughtException models unchecked exceptions
--
--   In order to declare an unchecked exception @E@,
--   all that is needed is to make @e@ an instance of 'UncaughtException'
--
--  > instance UncaughtException E
--
--   Note that declaring an exception E as unchecked does not automatically
--   turn its children unchecked too. This is a shortcoming of the current encoding.

class Exception e => UncaughtException e
instance UncaughtException e => Throws e NoExceptions
instance UncaughtException SomeException

-- ---------------
-- The EM Monad
-- ---------------

-- | A monad of explicitly typed, checked exceptions
type EM l = EMT l Identity

-- | Run a computation explicitly handling exceptions
tryEM :: EM AnyException a -> Either SomeException a
tryEM = runIdentity . tryEMT

tryEMWithLoc :: EM AnyException a -> Either (CallTrace, SomeException) a
tryEMWithLoc = runIdentity . tryEMTWithLoc

-- | Run a safe computation
runEM :: EM NoExceptions a -> a
runEM = runIdentity . runEMT

-- | Run a computation checking even unchecked (@UncaughtExceptions@) exceptions
runEMParanoid :: EM ParanoidMode a -> a
runEMParanoid = runIdentity . runEMTParanoid

instance (Throws MonadZeroException l) => MonadPlus (EM l) where
  mzero = throw MonadZeroException
  mplus = mplusDefault

-- -----------
-- Exceptions
-- -----------

-- | @FailException@ is thrown by Monad 'fail'
data FailException = FailException String deriving (Show, Typeable)
instance Exception FailException

-- | @MonadZeroException@ is thrown by MonadPlus 'mzero'
data MonadZeroException = MonadZeroException deriving (Show, Typeable)
instance Exception MonadZeroException

-- | This function may be used as a value for 'mplus' in 'MonadPlus'
mplusDefault :: Monad m => EMT l m a -> EMT l m a -> EMT l m a
mplusDefault emt1 emt2 = EMT$ do
                     v1 <- unEMT emt1
                     case v1 of
                       Left (_,CheckedException e) | Just MonadZeroException <- fromException e -> unEMT emt2
                       _  -> return v1
-- other

mapLeft :: (a -> b) -> Either a r -> Either b r
mapLeft f (Left x)  = Left (f x)
mapLeft _ (Right x) = Right x


instance MonadIO m => MonadIO (EMT l m) where
  liftIO = lift . liftIO