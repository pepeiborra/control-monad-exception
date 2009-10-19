{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module Control.Monad.Exception (
    EM,  tryEM, runEM, runEMParanoid,
    EMT, tryEMT, runEMT, runEMTParanoid,
    WithSrcLoc(..), withLocTH, showExceptionWithTrace,
    MonadZeroException(..),
    module Control.Monad.Exception.Class ) where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Exception.Class
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad.Cont.Class
import Control.Monad.RWS.Class
import Data.Monoid
import Data.Typeable
import qualified Language.Haskell.TH.Syntax
import Language.Haskell.TH.Syntax hiding (lift)
import Prelude hiding (catch)
import Text.PrettyPrint

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

-- | Run a safe computation checking even unchecked (@UncaughtExceptions@) exceptions
runEMParanoid :: EM ParanoidMode a -> a
runEMParanoid = runIdentity . runEMTParanoid

data MonadZeroException = MonadZeroException deriving (Show, Typeable)
instance Exception MonadZeroException

newtype EMT l m a = EMT {unEMT :: m (Either ([String], WrapException l) a)}

type AnyException = Caught SomeException

-- | Run explicitly handling exceptions
tryEMT :: Monad m => EMT (AnyException l) m a -> m (Either SomeException a)
tryEMT (EMT m) = mapLeft (wrapException.snd) `liftM` m

runEMT_gen :: Monad m => EMT l m a -> m a
runEMT_gen (EMT m) = liftM f m where
  f (Right x) = x
  f (Left  e) = error (uncurry showExceptionWithTrace e)

showExceptionWithTrace :: Show e => [String] -> e -> String
showExceptionWithTrace trace e = render$
             text (show e) $$
             text " in" <+> (vcat (map text $ reverse trace))

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

instance (Exception e, Throws e l, Monad m) => MonadThrow e (EMT l m) where
  throw = EMT . return . (\e -> Left ([],e)) . WrapException . toException
instance (Exception e, Monad m) => MonadCatch e (EMT (Caught e l) m) (EMT l m) where
  catchWithSrcLoc = catchEMT
  catch emt h = catchEMT emt (\_ -> h)

catchEMT :: (Exception e, Monad m) => EMT (Caught e l) m a -> ([String] -> e -> EMT l m a) -> EMT l m a
catchEMT emt h = EMT $ do
                v <- unEMT emt
                case v of
                  Right x -> return (Right x)
                  Left (trace, WrapException e) -> case fromException e of
                               Nothing -> return (Left (trace,WrapException e))
                               Just e' -> unEMT (h trace e')

-- | 'withLocTH' is a convenient TH macro which expands to 'withLoc' @\<source location\>@
--   Usage:
--
--  > f x = $withLocTH $ do
withLocTH :: Q Exp
withLocTH = do
  loc <- qLocation
  let loc_msg = showLoc loc
  [| withLoc loc_msg |]
 where
   showLoc Loc{loc_module, loc_filename, loc_start} = render $
                     {- text loc_package <> char '.' <> -}
                     text loc_module <> parens (text loc_filename) <> colon <+> text (show loc_start)

-- | Generating stack traces for exceptions
class WithSrcLoc a where
  -- | 'withLoc' records the given source location in the exception stack trace
  --   when used to wrap a EMT computation.
  --
  --   On any other monad or value, 'withLoc' is defined as the identity
  -- | hello
  withLoc :: String -> a -> a

instance WithSrcLoc a where withLoc _ = id

instance Monad m => WithSrcLoc (EMT l m a) where
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

instance (Throws SomeException l, MonadIO m) => MonadIO (EMT l m) where
  liftIO = lift . liftIO

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