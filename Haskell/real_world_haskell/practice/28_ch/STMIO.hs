import GHC.Conc (unsafeIOToSTM)
import Control.Concurrent.STM
import Control.Monad (join)

{-
 - The 'STM' monad forbids performing arbitrary I/O actions because they can
 - break the gurantees of atomicity and isolaton that the 'STM' monad
 - provides. However, often the need to perform I/O still arises.
 -
 - Most often, we will need to perform some I/O action as a result of a
 - decision we made inside an 'atomically' block. In these cases, the right
 - thing to do is usually to return a piece of data from 'atomically', which
 - will tell the caller in the 'IO' monad what to do next. We can even
 - return the action to perform, since actions are first class values (in
 - the form on an 'IO' monad).
 -}

someAction :: IO a
someAction = undefined

stmTransaction :: STM (IO a)
stmTransaction = return someAction

doSomething :: IO a
doSomething = join $ atomically stmTransaction

{-
 - Occasionally, we want to perform an I/O operation from within the 'STM'
 - monad. For instance, reading an immutable data from a file that must exit
 - does not violate the 'STM' gurantees of isolation and atomicity. In these
 - cases, we can use 'unsafeIOToSTM' (from GHC.Conc) to execute an 'IO' action:
 -   unsafeIOToSTM :: IO a -> STM a
 -
 - Note that this 'IO' action that will be lifted into 'STM' must not start
 - another 'atomically' transaction. If a thread tries to nest transactions
 - using 'unsafeIOToSTM', the runtime system will throw an exception. Since
 - the type system cannot gurantee that the 'IO' action will always obey the
 - rules, it is best to limit the use of 'unsafeIOToSTM'.
 -}

{-
 - An example of invalid usage of 'unsafeIOToSTM':
 -
 -   launchTorpedoes :: IO ()
 -   launchTorpedoes = ...
 -
 -   notActuallyAtomic = do
 -     doStuff
 -     unsafeIOToSTM launchTorpedoes
 -     mightRetry
 -
 - If the 'mightRetry' block causes the transaction to restart,
 - 'launchTorpedoes' will be called more than once. Since the runtime system
 - handles the retries for us, we cannot predict how many times
 - 'launchTorpedoes' will be called.
 -
 - Thus this can lead to unpredictable results and the solution is to not
 - perform these kinds of non-idempotent I/O actions inside a transaction.
 - how many times 'launchTorpedoes' may be called, 
 -}

