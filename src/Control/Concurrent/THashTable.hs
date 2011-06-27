-- |
-- Module     :  THashTable
-- The lock-free STM based hash table

module Control.Concurrent.THashTable (
   THashTable,
   empty,
   lookup,
   insert,
   delete,
   update,
   fromList,
   toList,
   hashString,
   hashInt
) where

import Data.Bits
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Control.Concurrent.STM
import Control.Monad (liftM, foldM)
import Data.Digest.Murmur32
import Data.Digest.Murmur64
import Data.Hashable
import Prelude hiding (lookup)
import qualified Prelude

type Hash = Int
type Bucket k v = HM.HashMap Hash [(k,v)]

data THashTable k v = THashTable {
   table :: V.Vector (TVar (Bucket k v)),
   hashFun :: k -> Hash
}

empty :: Int -> (k -> Hash) -> STM (THashTable k v)
empty capacity hashF = do
   let len = powerOf2 capacity
   bs <- V.replicateM len (newTVar HM.empty)
   return $ THashTable bs hashF
   where
     powerOf2 n = last $ takeWhile (<= n) (iterate (`shiftL` 1) 1) 

lookup :: (Eq k, Hashable k) => k -> THashTable k v -> STM (Maybe v)
lookup key tbl = do
   hm <- readTVar tvar
   return $! maybe Nothing (Prelude.lookup key) (look hm) 
   where
     hash = hashFun tbl key
     tvar = bucketFor hash tbl
     look = HM.lookup hash

insert :: (Eq k, Hashable k) => k -> v -> THashTable k v -> STM ()
insert key val tbl = do
   hm <- readTVar tvar
   writeTVar tvar (ins hm)
   where
     hash = hashFun tbl key
     tvar = bucketFor hash tbl
     ins = HM.insertWith (++) hash [(key,val)]

delete :: (Eq k, Hashable k) => k -> THashTable k v -> STM ()
delete key tbl = do
   hm <- readTVar tvar
   writeTVar tvar (del hm)
   where
     hash = hashFun tbl key
     tvar = bucketFor hash tbl
     vals hm = maybe [] clean (HM.lookup hash hm)
     clean = filter (\(k,_) -> k /= key)
     del hm = HM.insert hash (vals hm) hm

update :: (Eq k, Hashable k) => k -> v -> THashTable k v -> STM ()
update key val tbl = delete key tbl >> insert key val tbl

toList :: THashTable k v -> STM [(k,v)]
toList tbl = do
   hs <- mapM readTVar tvars
   let es = map HM.elems hs
   return $ items hs
   where
     tvars = V.toList (table tbl) 
     items hs = concat $ concatMap HM.elems hs

fromList :: (Eq k, Hashable k) => Int -> (k -> Hash) -> [(k,v)] -> STM (THashTable k v)
fromList cap hf ls = do
    tbl <- empty cap hf
    foldM (\t (k,v) -> insert k v t >> return t) tbl ls

hashString :: String -> Hash
hashString s = if has32bits then fromIntegral $ asWord32 $ hash32 s 
                else fromIntegral $ asWord64 $ hash64 s

hashInt :: Int -> Hash
hashInt d = if has32bits then fromIntegral $ asWord32 $ hash32 d 
             else fromIntegral $ asWord64 $ hash64 d

-- | Helpers
bucketFor :: Int -> THashTable k v -> TVar (Bucket k v)
bucketFor hash tbl = V.unsafeIndex buckets ind
   where
     ind = hash .&. (len - 1)
     buckets = table tbl
     len = V.length buckets

has32bits :: Bool
has32bits 
   | bitSize (undefined :: Int) <= 32 = True
   | otherwise = False
