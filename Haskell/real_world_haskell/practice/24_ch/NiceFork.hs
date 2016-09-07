module NiceFork
  ( ThreadManager
  , newManager
  , forkManaged
  , getStatus
  , waitFor
  , waitAll
  ) where

import Control.Concurrent
import Control.Exception (IOException, try)
import qualified Data.Map as M
import Control.Monad (join)

{-
 - GHC's runtime system treats the program's original thread of control
 - differently from other threads. When this thread finishes executing, the
 - runtime system considers the program as a whole to have completed. If any
 - other threads are executing at the time, they are terminated, similar to
 - daemon threads found in other programming languages.
 -}

data ThreadStatus = Running
                  | Finished            -- terminated normally
                  | Threw IOException     -- killed by an uncaught exception
                    deriving (Eq, Show)

newtype ThreadManager = Mgr (MVar (M.Map ThreadId (MVar ThreadStatus)))
  deriving (Eq)

-- | Create a new thread manager
newManager :: IO ThreadManager
newManager = Mgr <$> newMVar M.empty

{-
 - Signature of 'modifyMVar':
 -   modifyMVar :: MVar a -> (a -> IO (a, b)) -> IO b
 -}
-- | Create a new managed thread
forkManaged :: ThreadManager -> IO () -> IO ThreadId
forkManaged (Mgr mgr) body = modifyMVar mgr $ \m -> do
  state <- newEmptyMVar
  tid   <- forkIO $ do
    result <- try body

    {-
     - Note that the empty 'MVar' locally bound to the 'state' variable
     - indicates that the forked thread is running. Since the forked thread
     - exits by putting a value in the 'state MVar'
     -}
    putMVar state $ either Threw (const Finished) result

  return (M.insert tid state m, tid)

-- | Immediately return the status of a managed thread
getStatus :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
getStatus (Mgr mgr) tid = modifyMVar mgr $ \m ->
  case M.lookup tid m of
    Nothing -> return (m, Nothing)
    Just st -> tryTakeMVar st >>= \mst -> case mst of

                 {-
                  - Again, empty 'MVar' indicates thread still running.
                  - 'tryTakeMVar' returns 'Nothing' if mvar is empty instead
                  - of blocking the current process.
                  -
                  -   tryTakeMVar :: MVar a -> IO (Maybe a)
                  -}
                 Nothing  -> return (m, Just Running)
                 Just sth -> return (M.delete tid m, Just sth)

-- | Block until a specific managed thread terminates
waitFor, waitFor2 :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
waitFor (Mgr mgr) tid = do
  maybeDone <- modifyMVar mgr $ \m ->
    return $ case M.updateLookupWithKey (\_ _ -> Nothing) tid m of
      (Nothing, _) -> (m, Nothing)
      (done,   m') -> (m', done)

  case maybeDone of
    Nothing -> return Nothing
    Just st -> Just <$> takeMVar st

{-
 - The 'return' as the second element of the tuple as well as the 'return'
 - for the entire lambda expression results in nested 'IO' monads, in which
 - case we can collapse two into one via 'join'
 -}
waitFor2 (Mgr mgr) tid = join . modifyMVar mgr $ \m ->
  return $ case M.updateLookupWithKey (\_ _ -> Nothing) tid m of
    (Nothing, _ ) -> (m,  return Nothing)
    (Just st, m') -> (m', Just <$> takeMVar st)

-- | Block until all threads terminate
waitAll :: ThreadManager -> IO ()
waitAll (Mgr mgr) = modifyMVar mgr elems >>= mapM_ takeMVar
  where elems m = return (M.empty, M.elems m)
