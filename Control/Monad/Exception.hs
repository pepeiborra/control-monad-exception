{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

/Notes about type errors and exception hierarchies/

 * A type error of the form:

>    No instance for (UncaughtException MyException)
>      arising from a use of `g' at examples/docatch.hs:21:32-35
>    Possible fix:
>      add an instance declaration for (UncaughtException MyException)
>    In the expression: g ()

is the type checker saying:

\"hey, you are trying to run a computation which throws a @MyException@ without handling it, and I won't let you\"

Either handle it or declare @MyException@ as an 'UncaughtException'.

 * A type error of the form:

>    Overlapping instances for Throws MyException (Caught e NoExceptions)
>      arising from a use of `g' at docatch.hs:24:3-6
>    Matching instances:
>      instance (Throws e l) => Throws e (Caught e' l)
>        -- Defined at ../Control/Monad/Exception/Throws.hs:46:9-45
>      instance (Exception e) => Throws e (Caught e l)
>        -- Defined at ../Control/Monad/Exception/Throws.hs:47:9-44
>    (The choice depends on the instantiation of `e'
>    ...

 is due to an exception handler for @MyException@
missing a type annotation to pin down the type of the exception.

 * If your sets of exceptions are hierarchical then you need to
   teach 'Throws' about the hierarchy.

>                                                 --   TopException
>                                                 --         |
>   instance Throws MidException   TopException   --         |
>                                                 --   MidException
>   instance Throws ChildException MidException   --         |
>   instance Throws ChildException TopException   --         |
>                                                 --  ChildException


 * Stack traces are provided via the @MonadLoc@ class, and only
   for explicitly annotated program points.
   For now there is the TH macro 'withLocTH' to help with this.
   Eventually a preprocessor could be written to automatically insert calls
   to 'withLoc' at every do statement.

>       f () = $withLocTH $ throw MyException
>       g a  = $withLocTH $ f a
>
>       main = runEMT $ $withLocTH $ do
>       g () `catchWithSrcLoc` \loc (e::MyException) -> lift(putStrLn$ showExceptionWithTrace loc e)

>        -- Running main produces the output:

>       *Main> main
>       MyException
>        in Main(example.hs): (12,6)
>           Main(example.hs): (11,7)


-}
module Control.Monad.Exception (
    EM,  tryEM, runEM, runEMParanoid,
    EMT, tryEMT, runEMT, runEMTParanoid,
    throw, Control.Monad.Exception.catch,
    finally, onException, bracket,
    wrapException,
    showExceptionWithTrace,
    MonadZeroException(..), UncheckedIOException(..),

    -- reexports
    Exception(..), SomeException, Typeable,
    MonadFail(..),
    Throws, Caught, UncaughtException,
    withLoc, withLocTH,

) where

import Control.Applicative
import qualified Control.Exception as CE
import Control.Monad.Identity
import Control.Monad.Exception.Catch
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad.Cont.Class
import Control.Monad.RWS.Class
import Control.Monad.Failure
import Data.Monoid
import Data.Typeable
import Prelude hiding (catch)

-- | A monad of explicitly typed, checked exceptions
type EM l = EMT l Identity


mapLeft :: (a -> b) -> Either a r -> Either b r
mapLeft f (Left x)  = Left (f x)
mapLeft _ (Right x) = Right x

-- | Run a computation explicitly handling exceptions
tryEM :: EM (AnyException l) a -> Either SomeException a
tryEM = runIdentity . tryEMT

-- | Run a safe computation
runEM :: EM NoExceptions a -> a
runEM = runIdentity . runEMT

-- | Run a computation checking even unchecked (@UncaughtExceptions@) exceptions
runEMParanoid :: EM ParanoidMode a -> a
runEMParanoid = runIdentity . runEMTParanoid

data MonadZeroException = MonadZeroException deriving (Show, Typeable)
instance Exception MonadZeroException

newtype EMT l m a = EMT {unEMT :: m (Either ([String], CheckedException l) a)}

type AnyException = Caught SomeException

-- | Run a computation explicitly handling exceptions
tryEMT :: Monad m => EMT (AnyException l) m a -> m (Either SomeException a)
tryEMT (EMT m) = mapLeft (unwrapException.snd) `liftM` m

runEMT_gen :: Monad m => EMT l m a -> m a
runEMT_gen (EMT m) = liftM f m where
  f (Right x) = x
  f (Left (loc,e)) = error (showExceptionWithTrace loc (unwrapException e))

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

instance (Exception e, Throws e l, Monad m) => MonadFail e (EMT l m) where
  failure = throw

instance (Exception e, Monad m) => MonadCatch e (EMT (Caught e l) m) (EMT l m) where
  catchWithSrcLoc = Control.Monad.Exception.catchWithSrcLoc
  catch           = Control.Monad.Exception.catch

throw :: (Throws e l, Monad m) => e -> EMT l m a
throw = EMT . return . (\e -> Left ([],e)) . CheckedException . toException

catch :: (Exception e, Monad m) => EMT (Caught e l) m a -> (e -> EMT l m a) -> EMT l m a
catch emt h = Control.Monad.Exception.catchWithSrcLoc emt (\_ -> h)

catchWithSrcLoc :: (Exception e, Monad m) => EMT (Caught e l) m a -> ([String] -> e -> EMT l m a) -> EMT l m a
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
finally m sequel = do { v <- m `onException` sequel; sequel; return v}


-- | Like finally, but performs the second computation only when the first one
--   rises an exception
onException :: Monad m => EMT l m a -> EMT l m b -> EMT l m a
onException (EMT m) (EMT sequel) = EMT $ do
                                     ev <- m
                                     case ev of
                                       Left{}  -> do { sequel; return ev}
                                       Right{} -> return ev

bracket :: Monad m => EMT l m a        -- ^ acquire resource
                   -> (a -> EMT l m b) -- ^ release resource
                   -> (a -> EMT l m c) -- ^ computation
                   -> EMT l m c
bracket acquire release run = do { k <- acquire; run k `finally` release k }

wrapException :: (Exception e, Throws e' l, Monad m) => EMT (Caught e l) m a -> (e -> e') -> EMT l m a
wrapException m mkE = m `Control.Monad.Exception.catch` (throw . mkE)

showExceptionWithTrace :: Exception e => [String] -> e -> String
showExceptionWithTrace = showFailWithStackTrace

instance Monad m => MonadLoc (EMT l m a) where
    withLoc loc (EMT emt) = EMT $ do
                     current <- emt
                     case current of
                       (Left (tr, a)) -> return (Left (loc:tr, a))
                       _              -> return current

instance (Throws MonadZeroException l) => MonadPlus (EM l) where
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

data UncheckedIOException = forall e. Exception e => UncheckedIOException e deriving Typeable
instance Show UncheckedIOException where show (UncheckedIOException e) = show e
instance Exception UncheckedIOException
instance UncaughtException UncheckedIOException

instance (Throws UncheckedIOException l, MonadIO m) => MonadIO (EMT l m) where
  liftIO m = EMT (liftIO m') where
      m' = liftM Right m
            `CE.catch`
           \(e::SomeException) -> return (Left ([], CheckedException $ toException $ UncheckedIOException e))

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