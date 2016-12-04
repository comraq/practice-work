{-# LANGUAGE RankNTypes #-}

module Free2 where

{-
   Talk by Dan Piponi
   @link - https://www.youtube.com/watch?v=OGUuGL0AgYs
 -}

import Control.Arrow
import Control.Monad (join)
import Control.Monad.Reader
import Control.Monad.State
import Data.List (foldl')

{-
   Monoids -> Free Monoids
      |            |
      V            V
   Monads  -> Free Monads
 -}

class Magma m where
  mult :: m -> m -> m

newtype Sum = Sum { getSum :: Int }
  deriving Show

instance Magma Sum where
  mult = curry $ (getSum *** getSum) >>> uncurry (+) >>> Sum

accumulate :: Magma m => m -> [m] -> m
accumulate = foldl' mult

-- accumulate 0 [1, 2, 3] = 6
-- accumulate 10 [-10, 5] = 5

data FreeMagma a = Var a
                 | Tree (FreeMagma a) (FreeMagma a)

instance Magma (FreeMagma a) where
  mult = Tree

interpretMagma :: Magma b => (a -> b) -> FreeMagma a -> b
interpretMagma f (Var a)    = f a
interpretMagma f (Tree l r) = interpretMagma f l `mult` interpretMagma f r

{-
   class Monoid m where
     (<>)   :: m -> m -> m
     mempty :: m
 -}

instance Monoid Sum where
  mappend = mult
  mempty = Sum 0

(<>) :: Monoid m => m -> m -> m
(<>) = mappend

data FreeMonoidA a = VarA a
                   | MEmptyA
                   | TreeA (FreeMonoidA a) (FreeMonoidA a)

-- Does not satisfy!
--   Tree MEmptyA a /= a
instance Monoid (FreeMonoidA a) where
  mempty  = MEmptyA
  mappend = TreeA

newtype FreeMonoidB a = FreeMonoidB { getFreeMonoidB :: [a] }

instance Monoid (FreeMonoidB a) where
  mempty = FreeMonoidB mempty
  mappend = curry $ (getFreeMonoidB *** getFreeMonoidB) >>> uncurry mappend >>> FreeMonoidB

interpretMonoid :: Monoid m => (a -> m) -> FreeMonoidB a -> m
-- interpretMonoid f (FreeMonoidB xs) = foldMap f xs
interpretMonoid = curry $ (foldMap *** getFreeMonoidB) >>> app

data BankOp = Deposit Sum | Withdraw Sum

program = FreeMonoidB [Deposit (Sum 10), Withdraw (Sum 5), Withdraw (Sum 2), Deposit (Sum 3)]

interpretOp :: BankOp -> Sum
interpretOp (Deposit a) = a
interpretOp (Withdraw a) = Sum . negate $ getSum a

interpretBankOp :: FreeMonoidB BankOp -> Sum
interpretBankOp = interpretMonoid interpretOp

-- interpretBankOp program
-- > Sum {getSum = 6}

{-
   However, monoid are limited because of the "shape" of (<>):
     a <> b

   Thus, the join of a tree of monoid values must always combine 2 monoids.

   We want something like:
     a <> Container of b's

   In fact, we want different a's to accept different number of b's.
   Thus, each combination stage can combine arbitrary number of b's
   into a single result value.

   A solution is to make elements of the new structure be containers of values.
   Then, (<>) will no longer be a binary operation, but a unary operation that
   will only act on 1 value because argument is a value of type a containing b's

   Note: If the container a only contains 1 b, then it will be just like Monoids.

   Tree Diagram to Illustrate:
     where m = node
           top row are the starting elements
           bottom row value is the result

   > Monoid:
      [m,  m,  m]
        \ /   /
         m   /
          \ /
           m

   > MonoidyThing (where {} represents a container)
      {m} -of-> {m m} -of-> {m ... (arbitrary number of m's)}
        \        /           /
         \      /           /
          m ---            /
           \              /
            \            /
             m ---------
     > from left to right is a hierarchy of containers

   Requirements:
   - identity element:
     > similar to the monoid identity to be able to multiply by a single element
     > identity must be a container with 1 element inside
 -}

-- Sketch of the "newly drafted" Monoid
class Functor m => MonoidyThing m where
  identity :: a -> m a
  multiply :: m (m a) -> m a

{-
   MonoidyThing == Monad!

   class Applicative m => Monad m where
     return :: a -> m a
     join   :: m (m a) -> m a

   Laws:
     join x  = x >>= id
     x >>= f = join $ fmap f x

   > Just as multiplication in the free monoid build lists,
     multiplication in free monad build trees
   > Just as free monoids are built from some base type of elements,
     free monads should be built from some base container type,
     along with the identity container
 -}

-- Data Declaration with Equation Analogies:

-- X = 1 + a * X
data List a   = Nil  | Cons a (List a)

-- X = e + f X
data Free f a = Id a | Free (f (Free f a))

join' :: Functor f => Free f (Free f a) -> Free f a
join' (Id x) = x
join' (Free f) = Free $ join' <$> f

instance Functor f => Functor (Free f) where
  fmap f (Id x)   = Id $ f x
  fmap f (Free x) = Free $ fmap f <$> x

instance Functor f => Applicative (Free f) where
  pure = return
  f <*> a = f >>= \f' -> fmap f' a

instance Functor f => Monad (Free f) where
  return = Id
  (Id x)   >>= f = f x
  (Free x) >>= f = Free $ fmap (>>= f) x

interpretMonad :: (Functor f, Monad m)
               => (forall x. f x -> m x)
               -> (forall x. Free f x -> m x)
interpretMonad _ (Id x)   = return x
interpretMonad f (Free x) = join . f $ interpretMonad f <$> x

data Choice x = Choice x x

instance Functor Choice where
  fmap f (Choice a b) = Choice (f a) (f b)

choice :: Free Choice Bool
choice = Free (Choice (Id True) (Id False))

test :: Free Choice String
test = do
  a <- choice
  b <- choice
  return $ if a && b
    then "Both true"
    else "At least one false"

inter1 :: Choice x -> IO x
inter1 (Choice a b) = do
  c <- readLn :: IO Bool
  return $ if c then a else b

-- Result monad is an IO action that is readLn, test will effectively call
-- readLn twice and finally yield result
go1 = interpretMonad inter1 test

inter2 :: Choice x -> Reader Bool x
inter2 (Choice a b) = do
  c <- ask
  return $ if c then a else b

-- Result monad is a Reader that asks for the environment twice, then yield
-- result
go2 :: Bool -> String
go2 = runReader (interpretMonad inter2 test)

inter3 :: Choice x -> State [Bool] x
inter3 (Choice a b) = do
  x:xs <- get
  put xs
  return $ if x then a else b

-- Will only consume the first 2 boolean values of the [Bool] argument
-- Note: since "x:xs <- get" pattern matches against state [Bool], will
--       "pattern match exhausted" error if [Bool] is empty
go3 :: [Bool] -> String
go3 = evalState (interpretMonad inter3 test)

inter3' :: Free Choice x -> State [Bool] x
inter3' (Id a) = return a
inter3' (Free (Choice a b)) = do
  x:xs <- get
  put xs
  if x then inter3' a else inter3' b

go3' :: [Bool] -> String
go3' = evalState (inter3' test)
