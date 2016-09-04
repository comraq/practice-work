{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , GeneralizedNewtypeDeriving
  #-}

import SupplyClass
import RandomSupply

newtype Reader e a = R { runReader :: e -> a }

instance Functor (Reader e) where
  -- fmap f m = R $ \r -> f $ (runReader m) r
  fmap f m = m >>= return . f

instance Applicative (Reader e) where
  pure = return

  f <*> a = f >>= (<$> a)

instance Monad (Reader e) where
  -- return a = R $ \_ -> a
  return = R . const

  m >>= k = R $ \r -> runReader (k (runReader m r)) r

{-
 - Monad: (Reader e) == ((->) r)
 -
 - Explanation of (>>=):
 -   - reader monad is a function that takes an environment 'e' or 'r',
 -     and returns a result based on that environment
 -   - the environment is treated as an immutable value
 -   - thus, 'runReader' calls the contained function with 'r' to get return
 -     value, then calls 'k' to transform the returned value, and finally
 -     re-run the newly produced reader monad with 'runReader' and call it
 -     with 'r' once again
 -}

{-
 - Ask returns the environment passed as the reader function is the 'id'
 - function, which returns its input argument (in this case, the
 - environment)
 -
 - ex: runReader (ask >>= \x -> return (x * 3)) 2
 -     > 6
 -
 -     - ask yields '2' (the environment to the function passed to (>>=))
 -       which then multiplies the '2' by '3', thus finally resulting 6
 -}
ask :: Reader e e
ask = R id

newtype MySupply e a = MySupply { runMySupply :: Reader e a }
  deriving (Functor, Applicative, Monad)

instance MonadSupply e (MySupply e) where
  next = MySupply $ do
           v <- ask
           return $ Just v

  {-
   - Alternative definition for 'next':
   -
   - next = MySupply (Just `liftM` ask)
   -}

xy :: (Num s, MonadSupply s m) => m s
xy = do
  Just x <- next
  Just y <- next
  return $ x * y

runMS :: MySupply i a -> i -> a
runMS = runReader . runMySupply
