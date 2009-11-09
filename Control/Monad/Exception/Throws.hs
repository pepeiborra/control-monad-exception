{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances, FlexibleInstances #-}
{-|
Defines the @Throws@ binary relationship between types.
-}

module Control.Monad.Exception.Throws (
    Throws, Caught,
    CheckedException(..)
    ) where

#if __GLASGOW_HASKELL__ < 610
import Control.Exception.Extensible (Exception(..), SomeException)
#else
import Control.Exception (Exception(..), SomeException)
#endif
import Data.Typeable

-- | A type level witness of a exception handler.
data Caught e l


{-| @Throws@ is a type level binary relationship
    used to model a list of exceptions.

    There are two cases in which the user may want
    to add further instances to @Throws@.

     1. To encode subtyping.

     2. To declare an exception as unexplicit (a programming error).

    [/Subtyping/]
     As there is no way to automatically infer
     the subcases of an exception,  they have to be encoded
     manually mirroring the hierarchy defined in the defined
     'Exception' instances.
     For example,
     the following instance encodes that @MyFileNotFoundException@ is
     a subexception of @MyIOException@ :
     
 > instance Throws MyFileNotFoundException (Caught MyIOException l)

    'Throws' is not a transitive relation and every ancestor relation
      must be explicitly encoded.

>                                                            --   TopException
>                                                            --         |
>   instance Throws MidException   (Caught TopException l)   --         |
>                                                            --   MidException
>   instance Throws ChildException (Caught MidException l)   --         |
>   instance Throws ChildException (Caught TopException l)   --         |
>                                                            --  ChildException

     'SomeException' is automatically
      an ancestor of every other exception type.

    [/Programming Errors/]
     In order to declare an exception @E@ as a programming error, which should
     not be explicit nor checked, use a 'Throws' instance as follows:

>    instance Throws e l
-}

class Exception e => Throws e l

instance Throws e l  => Throws e (Caught e' l)
instance Exception e => Throws e (Caught e l)

-- | @SomeException@ is at the top of the exception hierarchy
--   .
--   Capturing SomeException captures every possible exception
instance Exception e => Throws e (Caught SomeException l)
instance Throws SomeException (Caught SomeException l) -- Disambiguation instance

-- Labelled SomeException
-- ------------------------
-- | @CheckedException@ adds a phantom type parameter @l@ to @SomeException@
newtype CheckedException l = CheckedException {checkedException::SomeException} deriving (Typeable)
instance Show (CheckedException l) where show (CheckedException e) = show e
