{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UndecidableInstances #-}

-- | 'EMT' liftings for the classes in the Monad Transformer Library
module Control.Monad.Exception.Mtl (module Control.Monad.Exception, Control.Monad.Exception.catch) where

import qualified Control.Exception as CE

import qualified Control.Monad.Exception
import Control.Monad.Exception hiding (catch)
import Control.Monad.Exception.Catch as Catch
import Control.Monad.Exception.Throws
import "mtl" Control.Monad.Cont.Class
import "mtl" Control.Monad.Error
import "mtl" Control.Monad.List
import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.State
import "mtl" Control.Monad.Writer
import "mtl" Control.Monad.RWS
import Data.Monoid
import Prelude hiding (catch)

instance (Throws MonadZeroException l) => MonadPlus (EM l) where
  mzero = throw MonadZeroException
  mplus emt1 emt2 = EMT$ do
                     v1 <- unEMT emt1
                     case v1 of
                       Left _  -> unEMT emt2
                       Right _ -> return v1


instance MonadTrans (EMT l) where lift = EMT . liftM Right

instance (Throws SomeException l, MonadIO m) => MonadIO (EMT l m) where
  liftIO m = EMT (liftIO m') where
      m' = liftM Right m
            `CE.catch`
           \(e::SomeException) -> return (Left ([], CheckedException e))

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



-- MonadCatch Instances
-- -------------------------------------------------------------------------

-- Commented out due to the problem of duplicated Monad instances for Either
-- instance (Error e) => MonadCatch e (Either e) (Either e) where catch m h = either h Right m
instance (Error e, Monad m) => MonadCatch e (ErrorT e m) (ErrorT e m) where catch = catchError

instance MonadCatch e m m' => MonadCatch e (ListT m) (ListT m') where catch (ListT m) h = ListT (Catch.catch m (runListT . h))
instance MonadCatch e m m' => MonadCatch e (ReaderT r m) (ReaderT r m') where catch (ReaderT m) h = ReaderT (\s -> Catch.catch (m s) ((`runReaderT` s) . h))

instance (Monoid w, MonadCatch e m m') => MonadCatch e (WriterT w m) (WriterT w m') where catch (WriterT m) h = WriterT (Catch.catch m (runWriterT . h))

instance MonadCatch e m m' => MonadCatch e (StateT s m) (StateT s m') where catch (StateT m) h = StateT (\s -> Catch.catch (m s) ((`runStateT` s) . h))

instance (Monoid w, MonadCatch e m m') => MonadCatch e (RWST r w s m) (RWST r w s m') where catch (RWST m) h = RWST (\r s -> Catch.catch (m r s) ((\m -> runRWST m r s) . h))
