{-# LANGUAGE CPP #-}

{-|
A Monad Transformer for explicitly typed checked exceptions, described in detail by:

  * Jose Iborra, \"Explicitly Typed Exceptions for Haskell\",
    PADL'10, January 2010, <http://dl.dropbox.com/s/lgm12trtl0swtra/PADL10.pdf?dl=1>

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

module Control.Monad.Exception.Pure (
-- * Important trivia
-- ** Hierarchies of Exceptions
-- $hierarchies

-- ** Unchecked exceptions
-- *** Unchecked exceptions
-- $unchecked

-- ** Stack Traces
-- $stacktraces

-- ** Understanding GHC errors
-- $errors

-- * The EM monad
    EM,  tryEM, runEM, runEMParanoid,

-- * The EMT monad transformer
    EMT(..), CallTrace, tryEMT, runEMT, runEMTParanoid, AnyException, UncaughtException,

-- ** The Throws type class
   Throws, Caught,

-- * Exception primitives
      throw, rethrow, catch, catchWithSrcLoc,
    finally, onException, bracket, wrapException,

    showExceptionWithTrace,
    FailException(..), MonadZeroException(..), mplusDefault,

-- * Reexports
    Exception(..), SomeException(..), Typeable(..),
    Failure(..),
#if !MIN_VERSION_failure(0,2,0)
    Try(..), NothingException(..),
    WrapFailure(..),
#endif
    MonadLoc(..), withLocTH
) where

import Control.Monad.Exception.Base
import Control.Monad.Exception.Throws
import Control.Monad.Exception.Catch (MonadCatch, Exception, SomeException, fromException)
import qualified Control.Monad.Exception.Catch
import Control.Failure
import Control.Monad.Loc
import Control.Monad.Loc.TH
import Data.Typeable

{- $hierarchies
 If your sets of exceptions are hierarchical then you need to
   teach 'Throws' about the hierarchy. See the documentation of
   'Throws' for more info.
-}
{- $unchecked
An exception @E@ can be declared as unchecked by making @E@ an instance of
   'UncaughtException'.

> instance UncaughtException E

   @E@ will still appear in the list of exceptions thrown
   by a computation but 'runEMT' will not complain if @E@ escapes the computation
   being run.

Also, 'tryEMT' allows you to run a computation regardless of the exceptions it throws.
-}


{- $stacktraces
 Stack traces are provided via the "MonadLoc" package.
   All you need to do is add the following pragma at the top of your Haskell
   source files and use do-notation:

>  { # OPTIONS_GHC -F -pgmF MonadLoc # }

   Only statements in do blocks appear in the stack trace.

   Example:

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


{- $errors
A type error of the form:

>    No instance for (UncaughtException MyException)
>      arising from a use of `g' at examples/docatch.hs:21:32-35
>    Possible fix:
>      add an instance declaration for (UncaughtException MyException)
>    In the expression: g ()

is the type checker saying:

\"hey, you are trying to run a computation which throws a @MyException@ without handling it, and I won't let you\"

Either handle it or declare @MyException@ as an 'UncaughtException'.

A type error of the form:

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
-}


-- | The catch primitive
catch :: (Exception e, Monad m) => EMT (Caught e l) m a -> (e -> EMT l m a) -> EMT l m a
catch emt h = catchWithSrcLoc emt (\_ -> h)

-- | Catch and exception and observe the stack trace.
--   If on top of the IO monad, this will also capture asynchronous exceptions
catchWithSrcLoc :: (Exception e, Monad m) => EMT (Caught e l) m a -> (CallTrace -> e -> EMT l m a) -> EMT l m a
catchWithSrcLoc emt h = EMT $ do
                v <- unEMT emt
                case v of
                  Right x -> return (Right x)
                  Left (trace, CheckedException e) -> handle e trace
       where handle e trace =
                      case fromException e of
                               Nothing -> return (Left (trace,CheckedException e))
                               Just e' -> unEMT (h trace e')

instance (Exception e, Monad m) => MonadCatch e (EMT (Caught e l) m) (EMT l m) where
  catchWithSrcLoc = catchWithSrcLoc
  catch           = catch

-- | Sequence two computations discarding the result of the second one.
--   If the first computation rises an exception, the second computation is run
--   and then the exception is rethrown.
finally :: Monad m => EMT l m a -> EMT l m b -> EMT l m a
finally m sequel = do { v <- m `onException` sequel; _ <- sequel; return v}


-- | Like finally, but performs the second computation only when the first one
--   rises an exception
-- TODO asynchronous exceptions! This needs to be moved to Monad
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
wrapException mkE m = m `catchWithSrcLoc` \loc e -> rethrow loc (mkE e)
