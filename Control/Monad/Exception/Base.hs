{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}

{-|
A Monad Transformer for explicitly typed checked exceptions.

The exceptions thrown by a computation are inferred by the typechecker
and appear in the type signature of the computation as 'Throws' constraints.

Exceptions are defined using the extensible exceptions framework of Marlow (documented in "Control.Exception"):

 * /An Extensible Dynamically-Typed Hierarchy of Exceptions/, by Simon Marlow, in /Haskell '06/.

  /Example/

  > data DivideByZero = DivideByZero deriving (Show, Typeable)
  > data SumOverflow  = SumOverflow  deriving (Show, Typeable)
  
  > instance Exception DivideByZero
  > instance Exception SumOverflow
  
  > data Expr = Add Expr Expr | Div Expr Expr | Val Double

  > eval (Val x)     = return x
  > eval (Add a1 a2) = do
  >    v1 <- eval a1
  >    v2 <- eval a2
  >    let sum = v1 + v2
  >    if sum < v1 || sum < v2 then throw SumOverflow else return sum
  > eval (Div a1 a2) = do
  >    v1 <- eval a1
  >    v2 <- eval a2
  >    if v2 == 0 then throw DivideByZero else return (v1 / v2)

  GHCi infers the following types
  
  > eval                                             :: (Throws DivideByZero l, Throws SumOverflow l) => Expr -> EM l Double
  > eval `catch` \ (e::DivideByZero) -> return (-1)  :: Throws SumOverflow l => Expr -> EM l Double
  > runEM(eval `catch` \ (e::SomeException) -> return (-1))
  >                                                  :: Expr -> Double
-}
module Control.Monad.Exception.Base where

import Control.Applicative
import Control.Monad.Exception.Catch
import Control.Monad.Loc
import Control.Failure
import Control.Monad.Fix
import Data.Typeable
import Prelude hiding (catch)

type CallTrace = [String]

-- | A Monad Transformer for explicitly typed checked exceptions.
newtype EMT l m a = EMT {unEMT :: m (Either (CallTrace, CheckedException l) a)}

-- | Run a computation explicitly handling exceptions
tryEMT :: Monad m => EMT AnyException m a -> m (Either SomeException a)
tryEMT (EMT m) = mapLeft (checkedException.snd) `liftM` m

tryEMTWithLoc :: Monad m => EMT AnyException m a -> m (Either (CallTrace, SomeException) a)
tryEMTWithLoc = liftM (mapLeft (\(l,ce) -> (l, checkedException ce))) . unEMT

runEMT_gen :: forall l m a . Monad m => EMT l m a -> m a
runEMT_gen (EMT m) = m >>= \x ->
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

instance (Exception e, Throws e l, Failure e m, Monad m) => WrapFailure e (EMT l m) where
  wrapFailure mkE m
      = EMT $ do
          v <- unEMT m
          case v of
            Right _ -> return v
            Left (loc, CheckedException (SomeException e))
                    -> return $ Left (loc, CheckedException $ toException $ mkE e)

instance (Exception e, Monad m) => MonadCatch e (EMT (Caught e l) m) (EMT l m) where
  catchWithSrcLoc = Control.Monad.Exception.Base.catchWithSrcLoc
  catch           = Control.Monad.Exception.Base.catch

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


-- | The catch primitive
catch :: (Exception e, Monad m) => EMT (Caught e l) m a -> (e -> EMT l m a) -> EMT l m a
catch emt h = Control.Monad.Exception.Base.catchWithSrcLoc emt (\_ -> h)

-- | Like 'Control.Monad.Exception.Base.catch' but makes the call trace available
catchWithSrcLoc :: (Exception e, Monad m) => EMT (Caught e l) m a -> (CallTrace -> e -> EMT l m a) -> EMT l m a
catchWithSrcLoc emt h = EMT $ do
                v <- unEMT emt
                case v of
                  Right x -> return (Right x)
                  Left (trace, CheckedException e) -> case fromException e of
                               Nothing -> return (Left (trace,CheckedException e))
                               Just e' -> unEMT (h trace e')


-- | Sequence two computations discarding the result of the second one.
--   If the first computation rises an exception, the second computation is run
--   and then the exception is rethrown.
finally :: Monad m => EMT l m a -> EMT l m b -> EMT l m a
finally m sequel = do { v <- m `onException` sequel; _ <- sequel; return v}


-- | Like finally, but performs the second computation only when the first one
--   rises an exception
onException :: Monad m => EMT l m a -> EMT l m b -> EMT l m a
onException (EMT m) (EMT sequel) = EMT $ do
                                     ev <- m
                                     case ev of
                                       Left{}  -> do { _ <- sequel; return ev}
                                       Right{} -> return ev

bracket :: Monad m => EMT l m a        -- ^ acquire resource
                   -> (a -> EMT l m b) -- ^ release resource
                   -> (a -> EMT l m c) -- ^ computation
                   -> EMT l m c
bracket acquire release run = do { k <- acquire; run k `finally` (release k) }

-- | Capture an exception e, wrap it, and rethrow.
--   Keeps the original monadic call trace.
wrapException :: (Exception e, Throws e' l, Monad m) => (e -> e') -> EMT (Caught e l) m a -> EMT l m a
wrapException mkE m = m `Control.Monad.Exception.Base.catchWithSrcLoc` \loc e -> rethrow loc (mkE e)

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

newtype Identity a = Identity{runIdentity::a} deriving (Eq, Ord, Show)
instance Monad Identity where
  return = Identity
  Identity a >>= f = f a

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
