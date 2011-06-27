module Main where
import Control.Concurrent.LockManagerIO

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Control.Concurrent.ThreadManager
import Test.HUnit
import System.Random

main = defaultMain tests
tests = [
        testGroup "The same transaction and the data item" 
           [testCase "locksRR" test_locksRR,
            testCase "locksWR" test_locksWR,
            testCase "locksWR" test_locksWW,
            testCase "locksWR" test_locksRW
           ],
         testGroup "Timeout"
           [testCase "timeout1" test_timeout1
           ],
         testGroup "The same data item"
           [testCase "trans100" test_trans100
           ],
         testGroup "The different data items and transactions"
           [testCase "items100" test_items100
           ]
        ]

test_locksRR = sameTidItem (Read,Read) (OK,Denied)
test_locksWR = sameTidItem (Write,Read) (OK,Denied)
test_locksRW = sameTidItem (Read,Write) (OK,OK)
test_locksWW = sameTidItem (Write,Write) (OK,Denied) -- lock conversion ?? 

sameTidItem l r = do
  lm <- mkLockManager
  let (item, tid, timeout) = (DataItem 1, 1, 0)
  r1 <- lock lm item tid (fst l) timeout
  assertEqual "Lock reply 1" (fst r) r1
  r2 <- lock lm item tid (snd l) timeout
  assertEqual "Lock reply 2" (snd r) r2
  unlockAll lm tid    

test_timeout1 = do
  tm <- make
  lm <- mkLockManager
  let (item, tid, timeout) = (DataItem 1, 1, 2)
  r1 <- lock lm item tid Write timeout
  assertEqual "Lock reply 1" OK r1
  fork tm $ do
    r2 <- lock lm item (tid+1) Write timeout
    assertEqual "Lock reply 2" Timeout r2
    unlockAll lm (tid+1)
  waitForAll tm 
  r1' <- lock lm item tid Write timeout
  assertEqual "Lock reply 3" Denied r1'
  unlockAll lm tid

test_trans100 = do
  tm <- make
  lm <- mkLockManager
  let (item, timeout, trans) = (DataItem 1, 2, 100)
  forM_ [1..trans] $ \tran -> fork tm $ do
    let mode = if tran `mod` 2 == 0 then Read else Write 
    r2 <- lock lm item tran mode timeout
    threadDelay 100000
    unlockAll lm tran
  waitForAll tm
  r1 <- lock lm item (trans + 1) Write timeout
  assertEqual "Lock reply 1" OK r1
  unlockAll lm (trans + 1)

test_items100 = do
  tm <- make
  lm <- mkLockManager
  let (items, trans, timeout) = (100, 10, 2)
  forM_ [1..trans] $ \tran -> fork tm $ do
    forM_ [1..items] $ \item -> do
      rval <- getStdRandom (randomR (0, 1)) :: IO Int
      let mode = if rval == 0 then Read else Write 
      r2 <- lock lm (DataItem item) tran mode timeout
      threadDelay 100000
      unlockAll lm tran
  waitForAll tm
  r1 <- lock lm (DataItem 1) 1 Write timeout
  assertEqual "Lock reply 1" OK r1
  unlockAll lm (trans + 1)


