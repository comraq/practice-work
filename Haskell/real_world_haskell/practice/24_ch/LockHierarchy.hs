import Control.Concurrent

{-
 - An example of deadlock using mvars:
 -
 - Two forked threads attempts to take two mvars in different order. Thus if
 - the first thread takes one mvar while the second thread takes the other
 - mvar, now when they try to acquire each other's mvar while still holding
 - onto their own, we run into a deadlock situation.
 -}

nestedModification :: MVar Int -> MVar Int -> IO ()
nestedModification outer inner = do
  modifyMVar_ outer $ \x -> do
    yield -- force this thread to temporarily yield the CPU
    modifyMVar_ inner $ \y -> return $ y + 1
    return $ x + 1

  putStrLn "done"

main :: IO ()
main = do
  a <- newMVar 1
  b <- newMVar 2
  forkIO $ nestedModification a b
  forkIO $ nestedModification b a
  return ()
