{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}

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

>                                                            --   TopException
>                                                            --         |
>   instance Throws MidException   (Caught TopException l)   --         |
>                                                            --   MidException
>   instance Throws ChildException (Caught MidException l)   --         |
>   instance Throws ChildException (Caught TopException l)   --         |
           >                                                 --  ChildException


 * Stack traces are provided via the "MonadLoc" package.
   All you need to do is add the following pragma at the top of your Haskell
   source files and use do-notation:

>  {-# OPTIONS_GHC -F -pgmF MonadLoc #-}

   Only statements in do blocks appear in the stack trace.

* Example:

>       f () = do throw MyException
>       g a  = do f a
>
>       main = runEMT $ do
>                g () `catchWithSrcLoc`
>                        \loc (e::MyException) -> lift(putStrLn$ showExceptionWithTrace loc e)

>        -- Running main produces the output:

>       *Main> main
>       MyException
>        in f, Main(example.hs): (1,6)
>           g, Main(example.hs): (2,6)
>           main, Main(example.hs): (5,9)
>           main, Main(example.hs): (4,16)

-}
module Control.Monad.Exception (

--  The EM monad
--    EM,  tryEM, runEM, runEMParanoid,

-- * The EMT monad transformer
    EMT(..), CallTrace, tryEMT, runEMT, runEMTParanoid, AnyException,

-- * Exception primitives
    throw, rethrow, Control.Monad.Exception.catch, Control.Monad.Exception.catchWithSrcLoc,
    finally, onException, bracket, wrapException,

    showExceptionWithTrace,
    FailException(..), MonadZeroException(..),

-- * The Try class for absorbing other error monads
    Try(..), NothingException(..),

-- * Reexports
    Exception(..), SomeException(..), Typeable(..),
    MonadFailure(..), WrapFailure(..),
    Throws, Caught, UncaughtException,
    withLoc, withLocTH,

) where

import Control.Applicative
import Control.Monad.Exception.Catch
import Control.Monad.Loc
import Control.Monad.Failure.Class
import Control.Monad.Fix
import Data.Typeable
import Text.PrettyPrint
import Prelude hiding (catch)

type CallTrace = [String]

newtype EMT l m a = EMT {unEMT :: m (Either (CallTrace, CheckedException l) a)}

type AnyException = Caught SomeException

-- | Run a computation explicitly handling exceptions
tryEMT :: Monad m => EMT (AnyException l) m a -> m (Either SomeException a)
tryEMT (EMT m) = mapLeft (checkedException.snd) `liftM` m

runEMT_gen :: forall l m a . Monad m => EMT l m a -> m a
runEMT_gen (EMT m) = m >>= \x ->
                     case x of
                       Right x -> return x
                       Left (loc,e) -> error (showExceptionWithTrace loc (checkedException e))

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

  fail s = EMT $ return $ Left ([], CheckedException $ toException $ FailException s)

  emt >>= f = EMT $ do
                v <- unEMT emt
                case v of
                  Left e  -> return (Left e)
                  Right x -> unEMT (f x)

instance Monad m => Applicative (EMT l m) where
  pure  = return
  (<*>) = ap

instance (Exception e, Throws e l, Monad m) => MonadFailure e (EMT l m) where
  failure = throw

instance (Exception e, Throws e l, Monad m) => WrapFailure e (EMT l m) where
  wrapFailure mkE m
      = EMT $ do
          v <- unEMT m
          case v of
            Right _ -> return v
            Left (loc, CheckedException (SomeException e))
                    -> return $ Left (loc, CheckedException $ toException $ mkE e)

instance (Exception e, Monad m) => MonadCatch e (EMT (Caught e l) m) (EMT l m) where
  catchWithSrcLoc = Control.Monad.Exception.catchWithSrcLoc
  catch           = Control.Monad.Exception.catch

instance Monad m => MonadLoc (EMT l m) where
    withLoc loc (EMT emt) = EMT $ do
                     current <- emt
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
catch emt h = Control.Monad.Exception.catchWithSrcLoc emt (\_ -> h)

-- | Like 'Control.Monad.Exception.catch' but makes the call trace available
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

-- | Capture an exception e, wrap it, and rethrow.
--   Keeps the original monadic call trace.
wrapException :: (Exception e, Throws e' l, Monad m) => (e -> e') -> EMT (Caught e l) m a -> EMT l m a
wrapException mkE m = m `Control.Monad.Exception.catchWithSrcLoc` \loc e -> rethrow loc (mkE e)

showExceptionWithTrace :: Exception e => [String] -> e -> String
showExceptionWithTrace [] e = show e
showExceptionWithTrace trace e = render$
             text (show e) $$
             text " in" <+> (vcat (map text $ reverse trace))
{-
-}
instance UncaughtException SomeException

-- -----------------------------------------------
-- The Try class for absorbing other error monads
-- -----------------------------------------------
data NothingException = NothingException deriving (Typeable, Show)
instance Exception NothingException

class Try m l where try :: Monad m' => m a -> EMT l m' a
instance Throws NothingException l => Try Maybe l where try = maybe (throw NothingException) return
instance (Exception e, Throws e l) => Try (Either e) l where try = either throw return

instance (Monad m, Try m l) => Try (EMT l m) l where try = join . fmap (EMT . return) .try . unEMT

-- -----------
-- Exceptions
-- -----------

-- | @FailException@ is thrown by Monad 'fail'
data FailException = FailException String deriving (Show, Typeable)
instance Exception FailException

-- | @MonadZeroException@ is thrown by MonadPlus 'mzero'
data MonadZeroException = MonadZeroException deriving (Show, Typeable)
instance Exception MonadZeroException


-- other

mapLeft :: (a -> b) -> Either a r -> Either b r
mapLeft f (Left x)  = Left (f x)
mapLeft _ (Right x) = Right x
