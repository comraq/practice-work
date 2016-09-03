module DList
  ( DList
  , fromList
  , toList
  , empty
  , append
  , cons
  , dfoldr
  ) where

import Utils

import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Seq
import Data.Sequence ((><), (<|), (|>))

newtype DList a = DL {
  unDL :: [a] -> [a]
}

append :: DList a -> DList a -> DList a
append xs ys = DL (unDL xs . unDL ys)

append' :: DList a -> DList a -> DList a
append' = curry $ DL . uncurry (.) . (unDL *** unDL)

append'' :: DList a -> DList a -> DList a
append'' (DL xs) (DL ys) = DL (xs . ys)

fromList :: [a] -> DList a
fromList xs = DL (xs ++)

fromList' :: [a] -> DList a
fromList' = DL . (++)

toList :: DList a -> [a]
toList (DL xs) = xs []

toList' :: DList a -> [a]
toList' = ($ []) . unDL

empty :: DList a
empty = DL id

-- equivalent of list type's (:) cons operator
infixr `cons`
cons :: a -> DList a -> DList a
cons x (DL xs) = DL ((x:) . xs)

infixr `cons'`
cons' :: a -> DList a -> DList a
cons' = curry
  $   ((:) *** unDL)
  >>> uncurry (.)
  >>> (DL $)

dfoldr :: (a -> b -> b) -> b -> DList a -> b
dfoldr f z xs = foldr f z (toList xs)

dfoldr' :: (a -> b -> b) -> b -> DList a -> b
dfoldr' = foldr *. (. toList)

safeHead :: DList a -> Maybe a
safeHead xs = case toList xs of
  (y:_) -> Just y
  _     -> Nothing

safeHead' :: DList a -> Maybe a
safeHead' =
  let justHeadOrNothing = ((Just . head) &&& const Nothing) >>> uncurry (<<?)
  in toList >>> (justHeadOrNothing &&& null) >>> app

dmap :: (a -> b) -> DList a -> DList b
dmap f = dfoldr go empty
  where go x xs = cons (f x) xs

instance Functor DList where
  fmap = dmap

instance Monoid (DList a) where
  mempty = empty
  mappend = append


