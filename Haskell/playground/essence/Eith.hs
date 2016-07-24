module Eith where

newtype Eith b = Eith { runEith :: Either String b }

instance Monad Eith where
  return    = Eith . Right
  fail      = Eith . Left
  m >>= f   = case runEith m of
    Left  e  -> Eith $ Left e
    Right v  -> f v

instance Applicative Eith where
  pure    = return
  f <*> a = f >>= \f' -> fmap f' a

instance Functor Eith where
  fmap f = Eith . fmap f . runEith
