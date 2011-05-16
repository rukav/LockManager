module Control.Concurrent.LockManagerSTM (
    LockMode (..),
    LockReply (..),
    Tid (..),
    Timeout (..),
    mkLockManager,
    lock,
    unlockAll,
    hashString,
    hashInt
) where

import Control.Concurrent.STM
import Control.Concurrent (forkIO, threadDelay, killThread)
import System.CPUTime (getCPUTime)
import Control.Monad (foldM)
import Control.Concurrent.MVar
import qualified Control.Concurrent.THashTable as T
import Control.Concurrent.THashTable
import Control.Concurrent.Chan
import Control.Monad (foldM_)
import Data.Hashable

data LockMode =
    Read
  | Write
  deriving (Eq, Show)

data LockReply =
    OK
  | Timeout
  | Deadlock
  | Denied
  deriving (Eq, Show)

data LockStatus =
    Granted
  | Expired
  | Wakeup
  | Converting
  deriving (Eq, Show)
  
type Tid = Int
type Timeout = Int

data Lock = Lock {
   tid :: Tid,
   mode :: LockMode
} deriving (Eq, Show)

data WaitLock = WaitLock {
   lck :: Lock,
   chan :: TChan LockStatus
}
   
data TableItem = TableItem {
   holds :: [Lock],
   waits :: [WaitLock]
}

type LockManager a = THashTable a TableItem

mkLockManager :: (a -> Int) -> IO (LockManager a)
mkLockManager hf = atomically $ empty 16 hf

startTimer :: Timeout -> IO (TChan LockStatus)
startTimer timeout = do
   c <- newTChanIO
   forkIO (timer c timeout)
   return c
   
timer :: TChan LockStatus -> Timeout -> IO ()
timer c timeout = do
   threadDelay (timeout * 1000000) 
   atomically $ writeTChan c Expired
   return ()

checkTimer :: TChan LockStatus -> IO LockStatus
checkTimer c = atomically $ readTChan c

lock :: (Eq a, Hashable a) => LockManager a -> a -> Tid -> LockMode -> Timeout -> IO LockReply
lock lm item tid mode timeout = do
      c <- startTimer timeout
      loop lm item (Lock tid mode) c

loop :: (Eq a, Hashable a) => LockManager a -> a -> Lock -> TChan LockStatus -> IO LockReply
loop lm item lk c = do
      reply <- atomically $ lock' lm item lk c
      case reply of
        Timeout -> do
            status <- checkTimer c
            case status of
               Expired -> return Timeout
               Wakeup -> loop lm item lk c
        _ -> return reply

lock' :: (Eq a, Hashable a) => LockManager a -> a -> Lock -> TChan LockStatus -> STM LockReply
lock' lm item lk c = do
          val <- T.lookup item lm
          case val of
            Nothing -> do 
                T.insert item (TableItem [lk] []) lm 
                return OK
            Just ti -> do
                let t = ti {waits = deleteWait ti}
                case conflict (holds t) lk of
                  Nothing -> T.update item t lm >> return Denied
                  Just False -> do
                     let t' = t {holds = holds t ++ [lk]}
                     T.update item t' lm >> return OK
                  Just True -> waitLock c t >> return Timeout
    where
       deleteWait t = filter (\w -> lck w /= lk) (waits t)
       waitLock c t = do
          let wl = WaitLock lk c
          let t' = t {waits = waits t ++ [wl]}
          T.update item t' lm

unlockAll :: (Eq a, Hashable a) => LockManager a -> Tid -> IO ()
unlockAll lm tran = atomically $ do
   items <- T.toList lm
   mapM_ unlock items
   where 
     unlock (item,ti) = do
        let hs = filter (\x -> tid x /= tran) (holds ti)
        mapM_ wakeup (waits ti)
        if null hs && null (waits ti) then
           T.delete item lm
         else 
           T.update item (ti {holds = hs}) lm
     wakeup w = writeTChan (chan w) Expired

conflict :: [Lock] -> Lock -> Maybe Bool
conflict [] lk = Just False
conflict (l:ls) lk 
   | tid lk == tid l = 
          case mode lk of
            Read -> Nothing
            Write -> if mode l == Write then Nothing
                      else conflict ls lk -- todo lock convertion 
   | otherwise = 
          case mode lk of
            Read -> if mode l == Write then Just True
                     else conflict ls lk
            Write -> Just True


