{-# LANGUAGE DeriveFunctor #-}

module RecursionSchemes where

-- @link - https://medium.com/@jaredtobin/practical-recursion-schemes-c10648ec1c29#.x4fxalo7t

import Data.Functor.Classes
import Data.Functor.Foldable
import Data.List.Ordered (merge)
import Prelude hiding (Foldable, succ)


-- Naive Data Structure
data Natural = Zero | Succ Natural


-- Parametrized Base Structures
data NatF r = ZeroF | SuccF r
  deriving Functor

instance Show1 NatF where
  liftShowsPrec _ _ _ ZeroF     = showString "ZeroF"
  liftShowsPrec f _ d (SuccF n) = showParen (d > 10)
    $ showString "SuccF "
    . f 11 n

data TreeF a r = EmptyF | LeafF a | NodeF r r
  deriving Functor

instance Show a => Show1 (TreeF a) where
  liftShowsPrec _ _ _ EmptyF      = showString "EmptyF"
  liftShowsPrec _ _ _ (LeafF a)   = showString $ "LeafF " ++ show a
  liftShowsPrec f _ d (NodeF l r) = showParen (d > 10)
    $ showString "NodeF "
    . f 11 l
    . showString " "
    . f 11 r


-- Convenience
type Nat    = Fix NatF
type List a = Fix (ListF a)
type Tree a = Fix (TreeF a)

zero :: Nat
zero = Fix ZeroF

succ :: Nat -> Nat
succ = Fix . SuccF

cons :: a -> List a -> List a
cons x xs = Fix (Cons x xs)

nil :: List a
nil = Fix Nil

empty :: Tree a
empty = Fix EmptyF

leaf :: a -> Tree a
leaf = Fix . LeafF

node :: Tree a -> Tree a -> Tree a
node = curry $ Fix . uncurry NodeF


{-
   Note the implementation of Fix:
     newtype Fix f = Fix (f (Fix f))

   fix :: f (Fix f) -> Fix f
   fix = Fix

   unfix :: Fix f -> f (Fix f)
   unfix (Fix f) = f

 -}

{-
   Data.Functor.Foldable introduces two fundamental type classes,
   "Recursive" and "Corecursive"

   Recursive instances corresponds to types that can be "fixed"
   Corecursive instances corresponds to types that can be "unfixed"

   project :: Recursive t   => t -> Base t t
   embed   :: Corecursive t => Base t t -> t
 -}

{-
   Definition of Base:
     type family Base t :: * -> *

   Fix instance of Base:
     type instance Base (Fix f) = f

   ie: replace all occurences of 'Base (Fix f)' with 'f'
   ex:
     cata :: (Base t a -> a) -> t a -> a

     for Fix:
       cata :: (Base (Fix f) a -> a) -> Fix f a -> a
       cata :: (f a -> a) -> Fix f a -> a

   Now, all machinery/tools that requires 'Base' will work on all instances of 'Base'
 -}

{-
   Common/Useful Recursion Schemes:
     catamorphism - generalized fold
     anamorphism  - generalized unfold
     hylomorphism - anamorphism followed by catamorphism
     paramorphism - generalized fold with access to the input argument
                    corresponding to the most recent state of computation
 -}


------- Catamorphisms -------
-- cata :: Recursive t => (Base t a -> a) -> t -> a

natsum :: Nat -> Int
natsum = cata alg where
  alg (SuccF n) = n + 1
  alg _         = 0

filterL :: (a -> Bool) -> List a -> List a
filterL p = cata alg where
  alg (Cons x xs)
    | p x       = cons x xs
    | otherwise = xs
  alg _ = nil


------- Anamorphisms -------
-- ana :: Corecursive t => (a -> Base t a) -> a -> t

nat :: Int -> Nat
nat = ana coalg where
  coalg n
    | n <= 0    = ZeroF
    | otherwise = SuccF $ n - 1


------- Paramorphisms -------
-- para :: Recursive t => (Base t (t, a) -> a) -> t -> a

natfac :: Nat -> Int
natfac = para alg where
  alg (SuccF (n, f)) = natsum (succ n) * f
  alg _              = 1

natpred :: Nat -> Nat
natpred = para alg where
  alg (SuccF (n, _)) = n
  alg _              = zero

tailL :: List a -> List a
tailL = para alg where
  alg (Cons _ (xs, _)) = xs
  alg _                = nil


------- Hylomorphisms -------
-- hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
-- Note:
-- > no requirements for 'Recursive' or 'Corecursive'
-- > just the f-algebra and f-coalgebra as arguments

mergeSort :: Ord a => [a] -> [a]
mergeSort = hylo alg coalg where
  alg (LeafF c)   = [c]
  alg (NodeF l r) = merge l r
  alg _           = []

  coalg []  = EmptyF
  coalg [x] = LeafF x
  coalg xs  = uncurry NodeF $ splitAt (length xs `div` 2) xs
