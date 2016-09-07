{-# LANGUAGE BangPatterns #-}
import Control.Concurrent
import Control.Exception (mask, SomeException, throw, catch)

-- Note that both 'MVar' and 'Chan' are non-strict

{-
 - The problem with the following function is that the forked child thread
 - was never forced to produce its computation and put its result in the
 - 'MVar'. Thus, the thread does not really start working until the parent
 - thread tries to take the value out of the 'MVar'
 -}
notQuiteRight :: IO ()
notQuiteRight = do
    mv <- newEmptyMVar
    forkIO $ expensiveComputationStricter mv
    someOtherActivity
    result <- takeMVar mv
    print result

  where someOtherActivity = return ()

expensiveComputationStricter :: MVar String -> IO ()
expensiveComputationStricter mv = do
  let a = "this is "
      b = "not really "
      c = "all that expensive"
  putMVar mv $ a ++ b ++ c

{-
 - An attempt at implemeting a strict versino of 'modifyMVarStrict'
 - Uses '!' to force to WHNF (weak head normal form)
 -}
modifyMVarStrict :: MVar a -> (a -> IO a) -> IO ()
modifyMVarStrict m io = mask $ \restore -> do
  a <- takeMVar m
  !b <- io a `catch` ((\e -> putMVar m a >> throw e) :: SomeException -> IO a)

  putMVar m a
