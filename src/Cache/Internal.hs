{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Cache.Internal where

import Prelude hiding (lookup)
import Data.Maybe
import qualified Data.HashMap.Lazy as H
import Data.Hashable
import Data.Sequence as S

data Cache k v = Cache {
      _limit :: Maybe Int,
      _size  :: Int,
      _cache :: H.HashMap k v,
      _queue :: Seq k
   } deriving (Show, Eq)
   

instance (Hashable k, Eq k) => IsCache Cache k where
   lookup k cache = H.lookup k $ _cache cache
   
   insert k v cache = (if needDump then dump 1 cache else cache) {
            _cache = H.insert k v $ _cache cache,
            _queue = (_queue cache) |> k,
            _size = 1 + _size cache
         }
      where needDump = isNothing (limit cache) || fromJust (limit cache) > size cache
      
   limit = _limit
   
   size  = _size
   
   setLimit lm cache = cache {_limit = lm}
    
   dumpOldest cache = cache {
         _cache = H.delete k _c,
         _queue = S.drop 1 _q,
         _size = size cache - 1
      } where 
         _c = _cache cache
         _q = _queue cache
         k = _queue cache `index` 0
      
   newCache lm = Cache {
      _limit = lm,
      _size = 0,
      _cache = H.empty,
      _queue = S.empty
   } 
   

class IsCache c k where   
   -- | @newCache size@. Set a max number of elements the cache will hold.
   --
   -- If the newly inserted element exceeds the size limit,
   -- the oldest element is removed from the cache.
   newCache :: Maybe Int -> c k v
   
   -- | Alias for @newCache Nothing@
   newUnlimitedCache :: c k v
   newUnlimitedCache = newCache Nothing
   
   -- | Lookup an element from the cache
   lookup :: k -> c k v -> Maybe v
   
   -- | Insert an element to the cache. 
   --
   -- If the newly inserted element exceeds the size limit,
   -- the oldest element is removed from the cache.
   insert :: k -> v -> c k v -> c k v
   
   -- | Remove the oldest key from the cache.
   dumpOldest :: c k v -> c k v
   
   -- | Return the current number of elements.
   size :: c k v -> Int
   
   -- | Return the max number of elements the cache can hold.
   limit :: c k v -> Maybe Int

   -- | Change the size limit.
   setLimit :: Maybe Int -> c k v -> c k v
   
   -- | @dump n cache@ removes n oldest elements from the cache.
   dump :: Int -> c k v -> c k v
   dump 0 cache = cache
   dump n cache = dump (n - 1) (dumpOldest cache)
 