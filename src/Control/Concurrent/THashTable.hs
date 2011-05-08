module Control.Concurrent.THashTable (
   THashTable,
   newIO,
   new,
   hashString,
   hashInt
   -- ...
) where

import Data.Bits
import Data.Word (Word)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Control.Concurrent.STM
import Data.Digest.Murmur32
import Data.Digest.Murmur64

type Hash = Int
type Bucket k v = HM.HashMap Hash [(k,v)]

data THashTable k v = THashTable {
   table :: V.Vector (TVar (Bucket k v)),
   hashFun :: k -> Hash
}

newIO :: Eq k => Int -> (k -> Hash) -> IO (THashTable k v)
newIO capacity hf = atomically $ new capacity hf

new :: Eq k => Int -> (k -> Hash) -> STM (THashTable k v)
new capacity hf = do
   let len = powerOf2 capacity
   bs <- V.replicateM len (newTVar HM.empty)
   return $ THashTable bs hf
   where
     powerOf2 n = last $ takeWhile (< n) (iterate (flip shiftL 1) 1) 

lookup :: Eq k => k -> THashTable k v -> STM (Maybe v)
lookup key tbl = do
   hm <- readTVar tvar
   return $! maybe Nothing (Prelude.lookup key) (look hm) 
   where
     hash = (hashFun tbl) key
     tvar = bucketFor hash tbl
     look = HM.lookup hash

insert :: Eq k => k -> v -> THashTable k v -> STM ()
insert key val tbl = do
   hm <- readTVar tvar
   writeTVar tvar (ins hm)
   where
     hash = (hashFun tbl) key
     tvar = bucketFor hash tbl
     ins = HM.insertWith (++) hash [(key,val)]

delete :: Eq k => k -> THashTable k v -> STM ()
delete key tbl = do
   hm <- readTVar tvar
   writeTVar tvar (del hm)
   where
     hash = (hashFun tbl) key
     tvar = bucketFor hash tbl
     vals hm = maybe [] clean (HM.lookup hash hm)
     clean = filter (\(k,_) -> k /= key)
     del hm = HM.insert hash (vals hm) hm

bucketFor :: Hash -> THashTable k v -> TVar (Bucket k v)
bucketFor hash tbl = V.unsafeIndex buckets ind
   where
     ind = hash .&. (V.length buckets) - 1
     buckets = table tbl

hashString :: String -> Int
hashString s = if has32bits then fromIntegral $ asWord32 $ hash32 s 
                else fromIntegral $ asWord64 $ hash64 s

hashInt :: Int -> Int
hashInt d = if has32bits then fromIntegral $ asWord32 $ hash32 d 
             else fromIntegral $ asWord64 $ hash64 d

has32bits :: Bool
has32bits 
   | bitSize (undefined :: Int) <= 32 = True
   | otherwise = False