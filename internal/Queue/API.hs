{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}

module Queue.API where

class Queue q t | q -> t where
  enqueue :: q -> t -> IO (Either String ())
  dequeue :: q -> IO (Either String t)
  close :: q -> IO ()
  open :: q -> IO ()

