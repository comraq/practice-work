{-# LANGUAGE DeriveFunctor
           , RankNTypes
  #-}

module Cayley where

import Prelude hiding (abs)

-- @link - https://begriffs.com/posts/2016-02-04-difference-lists-and-codennsity.html

{-
 - > replace mon(oid|ad) with mon(oid|ad) of functions
 - > replace recursive constructions with function composition/application
 -
 - rep :: m  -> mm
 - abs :: mm -> m
 -   where m are either monoid or monads
 -
 - Laws:
 -     abs . rep == id
 -   rep (x * y) == (rep x) @ (rep y)
 -
 -   where '*' and '@' are the multiplication operations before and after rep
 -}


{-
 - Consider reversing a list, naive implementation is slow because:
 -   (++) :: [a] -> [a] -> [a]
 -   (++) [] ys     = ys
 -   (++) (x:xs) ys = x : xs ++ ys
 -}

-- O(n^2) Time Complexity
rev :: [a] -> [a]
rev []     = []
rev (x:xs) = rev xs ++ [x]


------- Difference List -------

type EList a = [a] -> [a]

rep :: [a] -> EList a
rep = (++)
-- rep xs = (xs ++)

abs :: EList a -> [a]
abs = ($ [])
-- abs f = f []

{-
 - Note that laws hold:
 -      abs . rep == id
 -   rep (x ++ y) == (rep x) . (rep y)
 -}

revE :: [a] -> EList a
revE []     = rep []
revE (x:xs) = revE xs . rep [x]

-- O(1) Time Complexity
fastRev :: [a] -> [a]
fastRev = abs . revE

{-
 - Connection between lists ([]) and ELists, both are Monoids!
 -
 - instance Monoid (EList a) where
 -   mempty  = id
 -   mappend = (.)
 -
 - instance Monoid [x] where
 -   mempty  = []
 -   mappend = (++)
 -
 - Since both are monoids, we know that 'rep' and 'abs' are just monoid
 - homomorphisms, and are inverse of each other (ie: an isomorphism)
 -}

{-
 - Now for Cayley's Theorem from 1854:
 -   "Every monoid is isomorphic to a submonoid of its monoid of
 -   endomorphisms"
 -
 - When applied to groups:
 -   "Every group 'G' is isomorphic to a subgroup of the symmetric group
 -   acting on 'G'"
 -
 - Since symmetry groups are simply bijective functions taking a Set to
 - itself, we have the notion of a monoid of endomorphisms (a morphism with
 - the same source and target).
 -
 - Now, instead of (++), we can generalize the implementations of abs and
 - rep to all monoids.
 -}

type MEndo m = m -> m

mrep :: Monoid m => m -> MEndo m
mrep = mappend

mabs :: Monoid m => MEndo m -> m
mabs = ($ mempty)


-- Now for Monads

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show, Functor)

subst :: Tree a -> (a -> Tree b) -> Tree b
subst (Leaf x) k   = k x
subst (Node l r) k = Node (subst l k) (subst r k)

instance Monad Tree where
  return = Leaf
  (>>=)  = subst

instance Applicative Tree where
  pure    = return
  f <*> a = f >>= (<$> a)

sprout :: Int -> Int -> Tree Int
sprout n = \i -> Node (Leaf (n - 1 - i)) (Leaf (i + 1))

fullTree :: Int -> Tree Int
fullTree 1 = Leaf 1
fullTree n = fullTree (n - 1) >>= sprout n

zigzag :: Tree Int -> Int
zigzag = zig where

  zig :: Tree Int -> Int
  zig (Leaf n)   = n
  zig (Node l r) = zag l

  zag :: Tree Int -> Int
  zag (Leaf n)   = n
  zag (Node l r) = zig r

{-
 - Note:
 -   zigzag (fullTree 3) == zigzag (Leaf 1 >>= sprout 2 >>= sprout 3)
 -
 -   O(n^2) - Time Complexity
 -   because we must traverse the tree from the root node on every
 -   invocation of (>>=)
 -}

------- The Codensity Monad -------

-- Note: recall 'ContT'
--   newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }

newtype CodT m a = CodT { runCodt :: forall b. (a -> m b) -> m b }
  deriving Functor

-- This is the monad equivalent to the difference list
instance Monad (CodT m) where
  return x = CodT $ \c -> c x
  f >>= g  = CodT $ \c -> runCodt f (\a -> runCodt (g a) c)

instance Applicative (CodT m) where
  pure    = return
  f <*> a = f >>= (<$> a)

type Coden = CodT Tree


-- (.) is not polymorphic enough to satisfy the 'forall b.' constraint
-- for the below to definitions to be point free

codenRep :: Tree a -> Coden a
codenRep t = CodT $ (t >>=)

codenAbs :: Coden a -> Tree a
codenAbs c = runCodt c return

-- Laws still hold:
--   (codenAbs . codenRep) x
--   codenAbs $ CodT $ (x >>=)
--   codenAbs $ CodT (x >>=)
--   runCodt (CodT (x >>=)) return
--   (x >>=) return
--   x >>= return
--   x

-- Make tree constructors for the 'Coden' Monad!
leaf :: a -> Coden a
leaf = return

node :: Coden a -> Coden a -> Coden a
node (CodT c1) (CodT c2) = CodT $ \c3 -> Node (c1 c3) (c2 c3)

{-
 - Now have our homomorphism property:
 -   rep (Leaf i)   = leaf i
 -   rep (t >>= f)  = rep t >>= (rep . f)
 -   rep (Node l r) = node (rep l) (rep r)
 -}

sproden :: Int -> Int -> Coden Int
sproden n = \i -> node (leaf (n - 1 - i)) (leaf (i + 1))

fullCoden :: Int -> Coden Int
fullCoden 1 = leaf 1
fullCoden n = fullCoden (n - 1) >>= sproden n
