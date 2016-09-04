{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Supply
  ( Supply
  , next
  , runSupply
  ) where

import Control.Monad.State

{-
 - Note we are not exporting the 'S' constructor function
 -
 - With the 'GeneralizedNewtypeDeriving' pragma, we can also add 'Monad' to
 - the derivation to avoid the unwrapS ans rewrap boilerplate for
 - instantiation 'Supply' as a 'Monad' instance.
 -}
newtype Supply s a = S (State [s] a)

runSupply :: Supply s a -> [s] -> (a, [s])
runSupply (S m) xs = runState m xs

-- Wrap result in 'Maybe' in case the consumer asks for values more than the
-- list in the supply
next :: Supply s (Maybe s)
next = S $ do
  st <- get
  case st of
    []     -> return Nothing
    (x:xs) -> do put xs
                 return $ Just x

unwrapS :: Supply s a -> State [s] a
unwrapS (S s) = s

instance Functor (Supply s) where
  fmap f m = S $ f <$> unwrapS m

instance Applicative (Supply s) where
  pure     = return
  f <*> a  = f >>= (<$> a)

instance Monad (Supply s) where
  s >>= m = S (unwrapS s >>= unwrapS . m)
  return  = S . return

showTwo :: Show s => Supply s String
showTwo = do
  a <- next
  b <- next
  return $ show "a: " ++ show a ++ ", b: " ++ show b
