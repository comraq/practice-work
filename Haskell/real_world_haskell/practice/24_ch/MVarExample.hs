import Control.Concurrent

communicate :: IO ()
communicate = do
  m <- newEmptyMVar
  forkIO $ do
    v <- takeMVar m
    putStrLn ("received " ++ show v)

  putStrLn "sending"
  putMVar m "wake up!"

{-
 - Getters and Setters for 'MVar':
 -   putMVar  :: MVar a -> a -> IO ()
 -   takeMVar :: MVar a -> IO a
 -
 - Note that if we try to put a value into an 'MVar' that is already full,
 - our thread is put to sleep until another thread takes the value out.
 - Similarly, if we try to take a value from an empty 'MVar', our thread is
 - put to sleep until some other thread puts a value back in.
 -}

{-
 - To create a new 'MVar' with or without an initial value:
 -   newMVar      :: a -> IO (MVar a)
 -   newEmptyMVar :: IO (MVar a)
 -}
