{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | 'EMT' liftings for the classes in the monads-fd package
module Control.Monad.Exception.MonadsTF (module Control.Monad.Exception, Control.Monad.Exception.catch
                                        ) where

import qualified Control.Monad.Exception
import Control.Monad.Exception hiding (catch, Error)
import Control.Monad.Exception.Catch
import Control.Monad.Cont.Class
import Control.Monad.RWS.Class

import Control.Monad.Trans.Class
import Control.Monad.Trans.Error
import Control.Monad.Trans.List
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.State (StateT(..))
import Control.Monad.Trans.Writer (WriterT(..))
import Control.Monad.Trans.RWS (RWST(..))

import Data.Monoid
import Prelude hiding (catch)

instance MonadCont m => MonadCont (EMT l m) where
  callCC f = EMT $ callCC $ \c -> unEMT (f (\a -> EMT $ c (Right a)))

instance MonadReader m => MonadReader (EMT l m) where
  type EnvType (EMT l m) = EnvType m
  ask = lift ask
  local f m = EMT (local f (unEMT m))

instance MonadState m => MonadState (EMT l m) where
  type StateType (EMT l m) = StateType m
  get = lift get
  put = lift . put

instance (MonadWriter m) => MonadWriter (EMT l m) where
  type WriterType (EMT l m) = WriterType m
  tell   = lift . tell
  listen m = EMT $ do
               (res, w) <- listen (unEMT m)
               return (fmap (\x -> (x,w)) res)
  pass m   = EMT $ pass $ do
               a <- unEMT m
               case a of
                 Left  l     -> return (Left l, id)
                 Right (r,f) -> return (Right r, f)

instance (MonadRWS m) => MonadRWS (EMT l m)


-- MonadCatch Instances
-- -------------------------------------------------------------------------
instance (Error e) => MonadCatch e (Either e) (Either e) where catch m h = either h Right m
instance (Error e, Monad m) => MonadCatch e (ErrorT e m) (ErrorT e m) where catch = catchError

instance MonadCatch e m m' => MonadCatch e (ListT m) (ListT m') where catch (ListT m) h = ListT (catch m (runListT . h))
instance MonadCatch e m m' => MonadCatch e (ReaderT r m) (ReaderT r m') where catch (ReaderT m) h = ReaderT (\s -> catch (m s) ((`runReaderT` s) . h))

instance (Monoid w, MonadCatch e m m') => MonadCatch e (WriterT w m) (WriterT w m') where catch (WriterT m) h = WriterT (catch m (runWriterT . h))

instance MonadCatch e m m' => MonadCatch e (StateT s m) (StateT s m') where catch (StateT m) h = StateT (\s -> catch (m s) ((`runStateT` s) . h))

instance (Monoid w, MonadCatch e m m') => MonadCatch e (RWST r w s m) (RWST r w s m') where catch (RWST m) h = RWST (\r s -> catch (m r s) ((\m -> runRWST m r s) . h))
