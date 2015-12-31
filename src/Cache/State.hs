{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Maintainer  : ylilarry@gmail.com
Stability   : Experimental
Portability : GHC

This module has everything you need to cache a function @(a -> b)@

@
   let cache = newCache Nothing :: Cache Int Int
   let f = do
      withCache doSomethingSlow 3 -- Slow; the result is cached in a map using 3 as the key.
      withCache doSomethingSlow 3 -- Read from the cache.
      withCache doSomethingSlow 3 :: SCache Int Int
   evalState f cache 
@

-}
module Cache.State (
      Cache,
      SCacheT,
      SCache,
      IsSCacheT(..)
   ) where

import Prelude hiding (lookup)
import Cache.Internal as I
import Control.Monad.State

type SCacheT k v n = StateT (Cache k v) n v
type SCache k v = State (Cache k v) v
 

class (IsCache m k, Monad n) => IsSCacheT m k n where  
   -- | @withCache action x@, "x" is an argument for the action.
   -- 
   -- If "x" is a key in the cache, ignore the "action" and return the cached value;
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

instance (IsCache m k, Monad n) => IsSCacheT m k n

   