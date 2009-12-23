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

    There is only one case in which the user must
    add further instances to @Throws@.
    If your sets of exceptions are hierarchical then you need to
   teach 'Throws' about the hierarchy.

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

     Note that 'SomeException' is automatically an ancestor of every other exception type.

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
