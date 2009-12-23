{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverlappingInstances #-}


{-| Defines the (not so useful anymore) 'MonadCatch' type class.
-}

module Control.Monad.Exception.Catch (
       module Control.Monad,
       module Control.Monad.Exception.Throws,
       MonadCatch(..),
       Exception(..), SomeException(..),
       ) where

import Control.Monad

#if __GLASGOW_HASKELL__ < 610
import Control.Exception.Extensible (Exception(..), SomeException)
import qualified Control.Exception.Extensible as Control.Exception
#else
import Control.Exception (Exception(..), SomeException)
import qualified Control.Exception
#endif

import Control.Monad.Exception.Throws

import Prelude hiding (catch)

class (Monad m, Monad m') => MonadCatch e m m' | e m -> m', e m' -> m where
   catch   :: m a -> (e -> m' a) -> m' a
   catchWithSrcLoc :: m a -> ([String] -> e -> m' a) -> m' a
   catchWithSrcLoc m h = catch m (h [])

instance Exception e => MonadCatch e IO IO where
   catch   = Control.Exception.catch
