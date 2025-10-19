module FNotation.Internal.Util where

import Control.Monad.Error.Class

unwrap :: (Monad m, MonadError () m) => Maybe a -> m a
unwrap (Just x) = pure x
unwrap Nothing = throwError ()
