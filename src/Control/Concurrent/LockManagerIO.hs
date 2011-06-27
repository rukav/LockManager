module Control.Concurrent.LockManagerIO (
    LockMode (..),
    LockReply (..),
    Tid (..),
    Timeout (..),
    DataItem (..),
    mkLockManager,
    lock,
    unlockAll
) where

import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Concurrent.MVar
import qualified Data.Map as M
import System.CPUTime (getCPUTime)
import Control.Monad (foldM)

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
  | Waiting
  | Wakeup
  | Converting
  deriving (Eq, Show)
  
type Tid = Int
type Timeout = Int

data DataItem a = DataItem {
   id :: a
} deriving (Eq, Show, Ord)

data Lock = Lock {
   tid :: Tid,
   mode :: LockMode
} deriving (Eq, Show)

data WaitLock = WaitLock {
   lck :: Lock,
   notifier :: MVar LockStatus,
   delay :: Timeout,
   startTime :: Integer
} deriving Eq
   
data TableItem = TableItem {
   holds :: [Lock],
   waits :: [WaitLock]
}

type LockTable a = M.Map (DataItem a) (MVar TableItem)
newtype LockManager a = LM (MVar (LockTable a))
  deriving Eq

mkLockManager :: IO (LockManager a)
mkLockManager = LM `fmap` newMVar M.empty

lock :: Ord a => LockManager a -> DataItem a -> Tid -> LockMode -> Timeout -> IO LockReply
lock lm item tid mode t = loop (Just t)  where
   loop e = do
     reply <- lock' lm item (Lock tid mode) e
     case reply of
       Right r -> return r
       Left w -> do
         r <- wait w
         if r == Waiting then loop Nothing
          else waitLock w
   waitLock w = do
     now <- getCPUTime
     let diff = fromIntegral (now - startTime w) `div` (10^12)
     let timeout = delay w - diff
     loop (Just timeout)

lock' (LM lm) item lk timeout = 
   modifyMVar lm $ \m -> 
       case M.lookup item m of
          Nothing -> do
              tvar <- newEmptyMVar
              let t = TableItem [lk] []
              update m tvar t (Right OK)
          Just tvar -> do
              ti <- takeMVar tvar
              let t = ti {waits = delWaitLock lk (waits ti)}
              case timeout of
                 Nothing -> update m tvar t (Right Timeout)
                 Just tm -> 
                    case conflict (holds t) lk of
                        Nothing -> update m tvar t (Right Denied)
                        Just False -> do
                            let t' = t {holds = holds t ++ [lk]}
                            update m tvar t' (Right OK)
                        Just True -> do
                            wl <- mkWaitLock lk tm
                            let t' = t {waits = waits t ++ [wl]}
                            update m tvar t' (Left wl)
     where
       delWaitLock lk [] = []
       delWaitLock l (w:ws) = if l == lck w then ws else w : delWaitLock l ws 
       mkWaitLock lk tm = do
          now <- getCPUTime
          var <- newEmptyMVar
          return $ WaitLock lk var tm now
       update m tvar titem reply = do
          putMVar tvar titem
          return (M.insert item tvar m, reply)

-- todo
deadlock = undefined

unlockAll :: Ord a => LockManager a -> Tid -> IO ()
unlockAll (LM lm) tran = modifyMVar lm $ \m -> do
   m' <- foldM unlock m (M.toList m)
   return (m', ())
   where 
     unlock m (item,tvar) = do
        t <- takeMVar tvar
        let hs = filter (\x -> tid x /= tran) (holds t)
        mapM_ wakeup (waits t)
        putMVar tvar (t {holds = hs})
        if null hs && null (waits t) then
           return $ M.delete item m
         else return $ M.insert item tvar m

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

wait :: WaitLock -> IO LockStatus
wait w = do
   id <- forkIO $ do
      threadDelay (timeout * 1000000)
      putMVar var Waiting
   val <- takeMVar var
   killThread id
   return val
   where var = notifier w
         timeout = delay w

wakeup :: WaitLock -> IO Bool
wakeup w = tryPutMVar var Wakeup
   where var = notifier w

