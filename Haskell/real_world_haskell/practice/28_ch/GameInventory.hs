{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-
 - To support the traditional threaded model of concurrent programming,
 - Haskell has the 'MVar' mechanism to act as lock/communication between
 - threads. However it still suffers from the following problems:
 -
 - * Race conditions due to forgotten locks
 - * Deadlocks resulting from inconsistent lock ordering
 - * Corruption caused by uncaught exceptions
 - * Lost wakeups induced by omitted notifications
 -}

{-
 - Software transactional memory (STM) can help address some of the problems
 - mentioned above. By executing a block of actions as a transaction using
 - the 'atomically' combinator. Entering any such block, other threads
 - cannot see any modifications made within this block until the execution
 - of this block is finished and exited. At the same time, the thread within
 - the block cannot see any changes made by other threads. Thus, the
 - execution of a block is "isolated"
 -
 - Upon the exit from a transaction, exactly one of the following
 - things will occur:
 -
 - * If no other thread concurrently  modified the same data, all of the
 -   modifications within the transaction will simultaneously become visible
 -   to other threads
 -
 - * Otherwise, modifications in the transaction are discarded without being
 -   performed, and the block of actions are automatically restarted
 -}

import Control.Concurrent.STM
import Control.Monad
import Data.List (delete, find)

data Item = Scroll
          | Wand
          | Banjo
  deriving (Eq, Ord, Show)

newtype Gold = Gold Int
  deriving (Eq, Ord, Show, Num)

newtype HitPoint = HitPoint Int
  deriving (Eq, Ord, Show, Num)

{-
 - 'TVar' is a mutable variable that can be read or written inside an
 - 'atommically' block.
 -}
type Inventory = TVar [Item]
type Health    = TVar HitPoint
type Balance   = TVar Gold

data Player = Player {
  balance   :: Balance
, health    :: Health
, inventory :: Inventory
}

basicTransfer :: Gold -> Balance -> Balance -> STM ()
basicTransfer qty fromBal toBal = do
  fromQty <- readTVar fromBal
  toQty   <- readTVar toBal
  writeTVar fromBal $ fromQty - qty
  writeTVar toBal   $ toQty   + qty

transferTest :: STM (Gold, Gold)
transferTest = do
  alice <- newTVar (12 :: Gold)
  bob   <- newTVar 4
  basicTransfer 3 alice bob
  (,) <$> readTVar alice <*> readTVar bob

{-
 - If item to delete is found, return the updated maybe list with the
 - item removed. Otherwise Nothing.
 -}
removeInv :: Eq a => a -> [a] -> Maybe [a]
removeInv item inventory = (`delete` inventory) <$> find (== item) inventory

maybeGiveItem :: Eq a => a -> TVar [a] -> TVar [a] -> STM Bool
maybeGiveItem item fromInv toInv = do
  fromList <- readTVar fromInv
  case removeInv item fromList of
    Nothing      -> return False
    Just newList -> do
      writeTVar fromInv newList
      destItems <- readTVar toInv
      writeTVar toInv $ item : destItems
      return True

{-
 - To get an atomic transaction out of the 'STM' monad, Haskell has the
 - 'atomically' function:
 -   atomically :: STM a -> IO a
 -
 - Only in the 'STM' monad does all the transactional code execute.
 - Functions to operate within the 'STM' monad include the 'readTVar',
 - 'writeTVar' and etc...
 -
 - The 'STM' exists to prevent us from executing operations that may violate
 - the transaction gurantees, such as performing I/O or manipulate
 - non-transactional mutable state.
 -}

maybeSellItem :: Item -> Gold -> Player -> Player -> STM Bool
maybeSellItem item price buyer seller = do
  given <- maybeGiveItem item (inventory seller) (inventory buyer)
  if given
    then do
      basicTransfer price (balance buyer) (balance seller)
      return True
    else return False

{-
 - Note that the above transaction that may potentially fail is cumbersome
 - as we have to explicitly check for the success or failure and propate its
 - results back to the caller of the function.
 -
 - As an alternative, we can use the 'retry' function from the STM API as
 - follows:
 -}
giveItem :: Item -> Inventory -> Inventory -> STM ()
giveItem item fromInv toInv = do
  fromList <- readTVar fromInv
  case removeInv item fromList of
    Nothing      -> retry
    Just newList -> do
      writeTVar fromInv newList
      modifyTVar toInv (item:)

transfer :: Gold -> Balance -> Balance -> STM ()
transfer qty fromBal toBal = do
  fromQty <- readTVar fromBal
  when (qty > fromQty) retry

  writeTVar fromBal $ fromQty - qty
  modifyTVar toBal (qty +)

{-
 - Note that since we are using the 'transfer' defined above, instead of
 - returning 'False' immediately like in 'maybeSellItem', this function will
 - block (if necessary) until both the seller has the item adn the buyer had
 - enough money to pay for it.
 -}
sellItem :: Item -> Gold -> Player -> Player -> STM ()
sellItem item price buyer seller = do
  giveItem item (inventory seller) (inventory buyer)
  transfer price (balance buyer) (balance seller)

{-
 - The 'retry' function does not restart the transaction immediately.
 - Instead, it blocks the current thread until one or more ov the variables
 - that is used during the transaction is changed by another thread.
 -   ie: The current thread is suspended until some 'TVar' has been changed
 -       to so the current transaction will not waste CPU cycles retrying in the
 -       same failing environement
 -}

{-
 - However, sometimes we do not want to automatically 'retry' a transaction
 - upon failure as the desired outcome may just be try once or else fail.
 -
 - The 'orElse' combinator provides this by allowing a "backup" action if
 - the main transaction fails:
 -   orElse :: STM a -> STM a -> STM a
 -
 - Quote from the haddock documentation:
 -   Compose two alternative STM actions (GHC Only). If the first action
 -   completes without retrying then it forms the result of the 'orElse'.
 -   Otherwise, if the first action retries, then the second action is tried
 -   in its place. If both actions 'retry' then the 'orElse' as a whole
 -   retries.
 -}

{-
 - Suppose that we would like to buy the first item from a list that is both
 - in the possession of the seller and affordable (ie: have enough gold).
 - However, if the transaction fails, we want to stop /do nothing.
 -}
crummyList :: [(Item, Gold)] -> Player -> Player -> STM (Maybe (Item, Gold))
crummyList list buyer seller = go list
  where go []                         = return Nothing
        go (this@(item, price): rest) = do
            sellItem item price buyer seller
            return $ Just this

          `orElse`
            go rest

{-
 - Although the above works as we want, the 'crummyList' function suffers
 - from the problem of muddling together what we want to do and how we ought
 - to do it. We can see that there are two reusable patterns which can be
 - extracted out.
 -}

-- Pattern 1: Fail transaction immediately instead of retrying
maybeSTM :: STM a -> STM (Maybe a)
maybeSTM m = (Just <$> m) `orElse` return Nothing

{-
 - Pattern 2: Suppose we want to try an action over successive elements of a
 -            list, stopping at the first that succeeds, or performing a 'retry'
 -            if every one fails.
 -
 - We can use the fact that 'STM' is an instance of the 'MonadPlus' typeclass
 - defined as follows:
 -   instance MonadPlus STM where
 -     mzero = retry
 -     mplus = orElse
 -
 - Conveniently, in 'Data.Foldable', a 'msum' function is already defined on 'MonadPlus'
 - instances as follows:
 -   msum :: MonadPlus m => t (m a) -> m a
 -   msum = foldr mplus mzero
 -
 - Now we can write 'crummyList' in a much cleaner fashion as shown below.
 -}
shoppingList :: [(Item, Gold)] -> Player -> Player -> STM (Maybe (Item, Gold))
shoppingList list buyer seller = maybeSTM . msum $ map sellOne list
  where sellOne this@(item, price) = do
          sellItem item price buyer seller
          return this

-- Generalizing 'maybeSTM' for all 'MonadPlus' instances
maybeM :: MonadPlus m => m a -> m (Maybe a)
maybeM m = (Just <$> m) `mplus` return Nothing

main :: IO (Gold, Gold)
main = atomically transferTest

{-
 - If we transfer an amount greater than that is available, retry will be
 - stuck in an infinite loop, attempting to retry the transaction!
 -
 - > atomically $ transferTest2 13
 - > *** Exception: thread blocked indefinitely in an STM transaction
 -}
transferTest2 :: Int -> STM (Gold, Gold)
transferTest2 amount = do
  alice <- newTVar (12 :: Gold)
  bob   <- newTVar 4

  transfer (Gold amount) alice bob
  (,) <$> readTVar alice <*> readTVar bob

trySellItem :: Item -> Gold -> Player -> Player -> STM Bool
trySellItem item price buyer seller =
   sellItem item price buyer seller >> return True
 `orElse`
   return False

{-
 - One of the benefits of 'STM' is that it composes nicely. To add code to a
 - transaction, we just use our usual monadic building blocks such as (>>=)
 - and (>>). This is critical as the composability offered by 'STM' also
 - provides the gurantee that the actions in threaded programming are
 - atomic.
 -
 - The 'STM' monad prevents us from accidentally performing
 - non-transactional I/O actions. We do not need to worry about lock
 - ordering, since our code contains no locks. We can forget about lost
 - wakeups, since we do not have condition variables. If an exception is
 - thrown, we can also catch it use 'catchSTM' or be bounced out of the
 - transaction, leaving our state untouched! In addition, 'retry' and
 - 'orElse' also provide semantically meaningful functions to structure the
 - code.
 -
 - Although code that uses 'STM' will not deadlock, it is still possible for
 - threads to starve each other. A long-running transaction can cause
 - another transaction to 'retry' often enough that it will make
 - comparatively little progress. To address a proble like this, it is
 - advised to make transactions short.
 -
 - Like yielding explicit memory management to the garbage collector, by
 - using 'STM', we abandon the low level details in exchange for code that
 - we can better hope to understand.
 -}

{-
 - 'STM' cannot eliminate certain classes of bugs. For instance, if we
 - withdraw money from an account in one 'atomically' block, return to the
 - 'IO' monad, then deposit it to another account in a different
 - 'atomically' block, our code may still have an inconsistency. As, there
 - is the window between the two 'atomically' blocks where money is not
 - present in neither account.
 -
 - The functions below, 'bogusTransfer' and 'bogusSale', are examples to demonstrate.
 -}
bogusTransfer :: Gold -> Balance -> Balance -> IO ()
bogusTransfer qty fromBal toBal = do
  fromQty <- atomically $ readTVar fromBal

  -- window of inconsistency
  toQty   <- atomically $ readTVar toBal
  atomically $ writeTVar fromBal (fromQty - qty)

  -- window of inconsistency
  atomically $ writeTVar toBal   (toQty + qty)

bogusSale :: Item -> Gold -> Player -> Player -> IO ()
bogusSale item price buyer seller = do
  atomically $ giveItem item (inventory seller) (inventory buyer)
  bogusTransfer price (balance buyer) (balance seller)

{-
 - Invariants:
 -
 - When we create an invariant, it will immediately be checked. To fail, the
 - invariant must raise an exception. More interestingly, the invariant will
 - subsequently be checked automatically at the end of every transaction. If
 - it fails at any point, the transaction will be aborted and the exception
 - raised by the invariant will be propagated. This means that we will get
 - immediate feedback as soon as one of our invariants is violated.
 -}
newPlayer :: Gold -> HitPoint -> [Item] -> STM Player
newPlayer balance health inventory = Player
  <$> newTVar balance
  <*> newTVar health
  <*> newTVar inventory

populateWorld :: STM [Player]
populateWorld = sequence [ newPlayer 20 20 [Wand, Banjo]
                         , newPlayer 10 12 [Scroll]
                         ]

consistentBalance :: [Player] -> STM (STM ())
consistentBalance players = do
  initialTotal <- totalBalance
  return $ do
    curTotal <- totalBalance
    when (curTotal /= initialTotal) $
      error "inconsistent global balance"

  where totalBalance   = foldM addBalance 0 players
        addBalance a b = (a+) <$> readTVar (balance b)

{-
 - Because there is a temporary window in between transactions where 'Gold'
 - is extracted from the buyer but not yet credited to the seller,
 - 'tryBogusSale' will end us propagating the "inconsistent global balance"
 - error from 'consistentBalance'
 -}
tryBogusSale :: IO ()
tryBogusSale = do
  players@(alice:bob:_) <- atomically populateWorld
  atomically $ alwaysSucceeds =<< consistentBalance players
  bogusSale Wand 5 bob alice
