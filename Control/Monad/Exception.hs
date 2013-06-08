
{-|
A Monad Transformer for explicitly typed checked exceptions, described in detail by:

  * Jose Iborra, \"Explicitly Typed Exceptions for Haskell\",
    PADL'10, January 2010, <http://dl.dropbox.com/s/lgm12trtl0swtra/PADL10.pdf?dl=1>

"Control.Monad.Exception.Pure" provides the classic, Either based monad,
whereas "Control.Monad.Exception.IO" provides a more advanced IO based monad with
the ability to handle asynchronous exceptions as well.
-}

module Control.Monad.Exception ( module Control.Monad.Exception.Pure ) where

import Control.Monad.Exception.Pure