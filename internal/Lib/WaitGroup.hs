module WaitGroup where

import           Control.Concurrent.STM

data WaitGroup = WaitGroup
                   { count :: TVar Int
                   , lock  :: TMVar ()
                   }

newWaitGroup :: Int -> IO WaitGroup
newWaitGroup n = do
  c <- newTVarIO n
  WaitGroup c <$> newEmptyTMVarIO

add :: WaitGroup -> Int -> IO ()
add wg n = atomically $ modifyTVar' (count wg) (+n)

done :: WaitGroup -> IO ()
done wg = atomically $ do
  -- decrement the count
  c <- readTVar (count wg)
  case c of
    0 -> return ()
    1 -> modifyTVar' (count wg) (subtract 1) >> putTMVar (lock wg) ()
    _ -> modifyTVar' (count wg) (subtract 1)

wait :: WaitGroup -> IO ()
wait wg = atomically $ do
  -- wait for the lock to be released, then replace it
  l <- takeTMVar (lock wg)
  putTMVar (lock wg) l
