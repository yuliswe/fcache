{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Cache.State where

import Prelude hiding (lookup)
import Cache.Internal as I
import Control.Monad.State

class (IsCache m k, Monad n) => SCacheT m k n where  
   -- | @withCache cacheRef action x@, "x" is the argument of the action.
   -- 
   -- If "x" is a key in the cache, ignore the `action` and return the cached value;
   --
   -- Otherwise perform the action and cache the result using "x" as a key. 
   withCache :: (k -> n v) -> k -> StateT (m k v) n v
   withCache action k = do
      cache <- get
      case lookup k cache of
         Just v  -> return v
         Nothing -> do
            v <- lift $ action k
            modify (insert k v)
            return v

instance (IsCache m k, Monad n) => SCacheT m k n

   