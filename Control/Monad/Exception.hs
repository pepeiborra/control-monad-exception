
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

module Control.Monad.Exception (
-- * Important trivia
-- ** Hierarchies of Exceptions
-- $hierarchies

-- ** Unchecked and unexplicit exceptions
-- *** Unchecked exceptions
-- $unchecked

-- *** Unexplicit exceptions
-- $unexplicit

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
      throw, rethrow, Control.Monad.Exception.Base.catch, Control.Monad.Exception.Base.catchWithSrcLoc,
    finally, onException, bracket, wrapException,

    showExceptionWithTrace,
    FailException(..), MonadZeroException(..), mplusDefault,

-- * Reexports
    Exception(..), SomeException(..), Typeable(..),
    Try(..), NothingException(..),
    Failure(..), MonadFailure(..), WrapFailure(..),
    MonadLoc(..), withLocTH
) where

import Control.Exception (Exception(..), SomeException(..))
import Control.Monad.Exception.Base
import Control.Monad.Exception.Catch
import Control.Monad.Exception.Throws
import Control.Failure
import Control.Monad.Loc
import Data.Typeable

{- $hierarchies
 If your sets of exceptions are hierarchical then you need to
   teach 'Throws' about the hierarchy.

>                                                            --   TopException
>                                                            --         |
>   instance Throws MidException   (Caught TopException l)   --         |
>                                                            --   MidException
>   instance Throws ChildException (Caught MidException l)   --         |
>   instance Throws ChildException (Caught TopException l)   --         |
>                                                            --  ChildException
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
