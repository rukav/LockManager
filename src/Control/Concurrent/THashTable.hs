module Control.Concurrent.THashTable (
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

data THashTable k v = THashTable {
   buckets :: V.Vector (TVar (Bucket k v)),
   hashFun :: k -> Word
}

type Bucket k v = HM.HashMap Word [(k,v)]

newIO :: Eq k => Int -> (k -> Word) -> IO (THashTable k v)
newIO capacity hf = atomically $ new capacity hf

new :: Eq k => Int -> (k -> Word) -> STM (THashTable k v)
new capacity hf = do
   let len = powerOfTwoSize capacity
   bs <- V.replicateM len (newTVar HM.empty)
   return $ THashTable bs hf

bucketIndex :: Word -> THashTable k v -> Word
bucketIndex h t = h .&. toEnum (n - 1)
   where n = V.length $ buckets t 

powerOfTwoSize :: Int  -> Int
powerOfTwoSize n = last $ takeWhile (< n) (iterate (flip shiftL 1) 1) 

hashString :: String -> Word
hashString s = if has32bits then fromIntegral $ asWord32 $ hash32 s 
                else fromIntegral $ asWord64 $ hash64 s

hashInt :: Int -> Word
hashInt d = if has32bits then fromIntegral $ asWord32 $ hash32 d 
             else fromIntegral $ asWord64 $ hash64 d

has32bits :: Bool
has32bits 
   | bitSize (undefined :: Int) <= 32 = True
   | otherwise = False