module Profunctor where

import Utils

------- Functor (Covariant) -------

{-
 - class Functor f where
 -   fmap :: (a -> b) -> f a -> f b
 -}

newtype Burrito filling = Tortilla { getFilling :: filling }

instance Functor Burrito where
  fmap = (Tortilla .) . (. getFilling)
  -- fmap = (. getFilling) >>> (Tortilla .)
  -- fmap f = Tortilla . f . getFilling


------- Contravariant -------
{-
 - Customers only eat 'cooked' food.
 -
 - Customers can only be fed when cooked food (f b) is available.
 - Suppose now there is a way to 'cook' food (f :: a -> b)
 -   (ie: uncooked food -> cooked food).
 -
 - Customers can now be fed when there is uncooked food (f a).
 -}

class Contravariant f where
  cmap :: (a -> b) -> f b -> f a

newtype Customer filling = Customer { eat :: filling -> IO () }

instance Contravariant Customer where
  cmap = (Customer .) . (. eat) . flip (.)
  -- cmap f = Customer . (. f) . eat
  -- cmap g (Customer f) = Customer $ f . g

{-
 - Op is the function arrow where the parameter is fixed on the return type.
 - Input argument is the variant.
 -}
newtype Op b a = Op { getOp :: a -> b }

instance Contravariant (Op b) where
  cmap = (Op .) . (. getOp) . flip (.)

{-
 - Comparison between 'Covariant Functors' and 'Contravariants':
 -
 - - Type arguments of 'Covariant Functors' appear only in the positive
 -   position
 - - Type arguments of 'Contravariant' appear only in the the negative
 -   position
 -
 -   ex: (negative positions are marked with primed "'" and does not
 -        indicate a difference in type)
 -     - a
 -     - a' -> a
 -     - a' -> a' -> a
 -       Note: the first 2 'a's are negative, think of a -> a -> a as uncurried:
 -             (a, a) -> a
 -     - (a -> a') -> a
 -       Note: the second 'a' is still negative but the first 'a' is positive,
 -             because we need to travese 2 function arrows from the
 -             rightmost 'a' to the leftmost 'a', in which we encounter
 -             2 inversions of polarity, ~ . ~ === id
 -               where (~) === negation
 -     - ((a' -> a) -> a') -> a
 -
 -   Note: If the type argument is only in positive positions, then the type
 -         argument varies 'covariantly' (we can instantiate a 'covariant
 -         Functor').
 -         If only in negative positions, then the type argument varies
 -         'contravariantly' (we can instantiate a 'contravariant').
 -         If the type argument are in both positive and negative positions,
 -         then no 'covariant functors' nor 'contravariants' can be
 -         instantiated
 -
 -
 - - cmap over contravariant (-> z) 'extends' the function on its input/argument
 -   type.
 -   - fmap (a -> b) >>> fmap (b -> c)
 -   - f a -> f b -> f c and ...
 -
 - - fmap over covariant ((->) r) 'extends' the function on its output/return
 -   type.
 -   - cmap (b -> c) >>> cmap (a -> b)
 -   - f c -> f b -> f a and ...
 -}

class Invariant f where
  imap :: (b -> a) -> (a -> b) -> f a -> f b

newtype Endo a = Endo { getEndo :: a -> a }

instance Invariant Endo where
  imap f = (.) >>> (. ((. f) . getEndo)) >>> (Endo .)
  -- imap f g (Endo e) = Endo (g . e . f)

-- Isomorphism, a pair of functions that are 'inverses' (dual) to each other
data Iso a b = Iso (a -> b) (b -> a)

iso :: Invariant f => Iso a b -> f a -> f b
iso (Iso to from) = imap from to

class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d

instance Profunctor (->) where
  dimap = flip (.) >>> flip (.*)
  -- dimap f g k = g . k . f

{-
 - Dimap maps 'contravariantly' with its first argument
 - - 'extends' the input/argument from b to a with (a -> b)
 -
 - Dimap then maps 'covariantly' with its second argument
 - - 'extends' the output/return from c to d with (c -> d)
 -}

isoP :: Profunctor p => Iso a b -> p b b -> p a a
isoP (Iso to from) = dimap to from
