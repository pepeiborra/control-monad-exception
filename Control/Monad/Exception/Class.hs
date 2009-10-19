{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverlappingInstances #-}


{-| Defines 'MonadThrow' and 'MonadCatch' type classes and suitable instances for 'IO' and 'Either'.
-}

module Control.Monad.Exception.Class (
       module Control.Monad,
       module Control.Monad.Exception.Throws,
       MonadThrow(..), MonadCatch(..),
       WrapException(..), Exception(..), SomeException(..),
       showExceptionWithTrace
       ) where

import Control.Monad
#if TRANSFORMERS
import Control.Monad.Trans
import Control.Monad.Trans.Error
import Control.Monad.Trans.List
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Control.Monad.Trans.RWS
#else
import Control.Monad.Error
import Control.Monad.List
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.RWS
#endif

#if __GLASGOW_HASKELL__ < 610
import Control.Exception.Extensible (Exception(..), SomeException)
import qualified Control.Exception.Extensible as Control.Exception
#else
import Control.Exception (Exception(..), SomeException)
import qualified Control.Exception
#endif
import Data.Typeable
import Text.PrettyPrint

import Control.Monad.Exception.Throws

import Prelude hiding (catch)

class Monad m => MonadThrow e m where
    throw :: e -> m a
    throwWithSrcLoc :: String -> e -> m a
    throwWithSrcLoc _ = throw

instance Exception e => MonadThrow e IO where
   throw = Control.Exception.throw

class (Monad m, Monad m') => MonadCatch e m m' | e m -> m', e m' -> m where
   catch   :: m a -> (e -> m' a) -> m' a
   catchWithSrcLoc :: m a -> ([String] -> e -> m' a) -> m' a
   catchWithSrcLoc m h = catch m (h [])

instance Exception e => MonadCatch e IO IO where
   catch   = Control.Exception.catch

{-| Given a list of source locations and an exception, @showExceptionWithTrace@ produces output of the form

>       <exception details>
>        in <module a>(<file a.hs>): (12,6)
>           <module b>(<file b.hs>): (11,7)
>           ...

-}
showExceptionWithTrace :: Exception e => [String] -> e -> String
showExceptionWithTrace trace e = render$
             text (show e) $$
             text " in" <+> (vcat (map text $ reverse trace))

-- Labelled SomeException
-- ------------------------
-- | @WrapException@ adds a phantom type parameter @l@ to @SomeException@
newtype WrapException l = WrapException {unwrapException::SomeException} deriving (Typeable)
instance Show (WrapException l) where show (WrapException e) = show e

-- Throw and Catch instances for the Either and ErrorT monads
-- -----------------------------------------------------------
instance (Error e) => MonadThrow e (Either e) where throw = Left
instance (Error e) => MonadCatch e (Either e) (Either e) where catch m h = either h Right m

instance (Error e, Monad m) => MonadThrow e (ErrorT e m) where throw = throwError
instance (Error e, Monad m) => MonadCatch e (ErrorT e m) (ErrorT e m) where catch = catchError


-- Instances for transformers (requires undecidable instances in some cases)
-- -------------------------------------------------------------------------
instance MonadThrow e m    => MonadThrow e (ListT m)            where throw = lift . throw
instance MonadCatch e m m' => MonadCatch e (ListT m) (ListT m') where catch (ListT m) h = ListT (catch m (runListT . h))

instance MonadThrow e m    => MonadThrow e (ReaderT r m)                where throw = lift . throw
instance MonadCatch e m m' => MonadCatch e (ReaderT r m) (ReaderT r m') where catch (ReaderT m) h = ReaderT (\s -> catch (m s) ((`runReaderT` s) . h))

instance (Monoid w, MonadThrow e m)    => MonadThrow e (WriterT w  m)               where throw = lift . throw
instance (Monoid w, MonadCatch e m m') => MonadCatch e (WriterT w m) (WriterT w m') where catch (WriterT m) h = WriterT (catch m (runWriterT . h))

instance MonadThrow e m    => MonadThrow e (StateT s m)               where throw = lift . throw
instance MonadCatch e m m' => MonadCatch e (StateT s m) (StateT s m') where catch (StateT m) h = StateT (\s -> catch (m s) ((`runStateT` s) . h))

instance (Monoid w, MonadThrow e m)    => MonadThrow e (RWST r w s m)                 where throw = lift . throw
instance (Monoid w, MonadCatch e m m') => MonadCatch e (RWST r w s m) (RWST r w s m') where catch (RWST m) h = RWST (\r s -> catch (m r s) ((\m -> runRWST m r s) . h))
