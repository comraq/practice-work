import Control.Applicative
import Data.Function (on)
import Data.Map as Map

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- type Predicate a = a -> Bool

{- Covariant Functor -}
{-
 - class Functor f where
 -   fmap :: (a -> b) -> f a -> f b
 -
 - Intuition:
 -
 -       f ::   a ->   b
 -  fmap f :: f a -> f b
 -}

{- Contravariant Functor -}
class Contravariant f where
  contramap :: (b -> a) -> f a -> f b


newtype Predicate a = Predicate { getPredicate :: a -> Bool }

instance Contravariant Predicate where
  contramap g (Predicate p) = Predicate (p . g)

{-
 - Intuition:
 -
 -       g ::   a <-   b
 -  fmap g :: f a -> f b
 -}

veryOdd :: Predicate Integer
veryOdd =  contramap (`div` 2) (Predicate odd)

main1 :: IO ()
main1 =  print $ getPredicate veryOdd <$> [0..11] -- [False, False, True, True, False, False, True, True, False, False, True, True]


-- From Control.Applicative:
--   newtype Const a b = Const a

instance Contravariant (Const a) where
  contramap _ (Const a) = Const a

newtype Comparison a = Comparison (a -> a -> Ordering) -- ie: compare

-- on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
instance Contravariant Comparison where
  contramap g (Comparison comp) = Comparison (comp `on` g)

newtype Op b a = Op (a -> b)
instance Contravariant (Op b) where
  contramap g (Op f) = Op (f . g)

class Bifunctor f where
  bimap :: (a -> c) -> (b -> d) -> f a b -> f c d

{-
 - Intuition:
 -
 -       g   ::   a   ->   c
 -         h ::     b ->     d
 - bimap g h :: f a b -> f c d
 -}

instance Bifunctor Either where
  bimap g h = either (Left . g) (Right . h)

instance Bifunctor (,) where
  {- bimap = (***) -- import from Control.Arrow -}
  bimap g h = \(b, b') -> (g b, h b')

class Profunctor f where
  dimap :: (c -> a) -> (b -> d) -> f a b -> f c d

{-
 - Intuition:
 -
 -       g   ::   a   <-   c
 -         h ::     b ->     d
 - bimap g h :: f a b -> f c d
 -}

lmap :: Profunctor f => (c -> a) -> f a b -> f c b
lmap =  (`dimap` id)

rmap :: Profunctor f => (b -> d) -> f a b -> f a d
rmap =  (id `dimap`)

instance Profunctor (->) where
  dimap g h f = h . f . g

type Limits a = Limits' a a
data Limits' a b = Limits { step :: a -> (b, b)
                          , check :: a -> a -> Bool }

parallel            :: (b -> b') -> (c -> c') -> (b, c) -> (b', c')
parallel f g (b, c) =  (f b, g c)

instance Profunctor Limits' where
  dimap g h lim = Limits { step  = (h `parallel` h) . step lim . g
                         , check = check lim `on` g }

maybeLimit   :: a -> Limits a -> Limits (Maybe a )
maybeLimit d =  dimap (maybe d id) Just

millionsLimit :: Limits Double -> Limits Double
millionsLimit =  dimap (1.0e6 *) (/ 1.0e6)

newtype Indexed i a b = Indexed { runIndexed :: i -> a -> b }

instance Profunctor (Indexed i) where
  dimap g h (Indexed f) = Indexed (dimap g h . f)

class Indexable i p where
  indexed :: p a b -> i -> a -> b

instance Indexable i (Indexed i) where
  indexed = runIndexed

instance Indexable i (->) where
  indexed = const

mapIndexable :: Indexable i p => p a b -> Map i a -> Map i b
mapIndexable =  Map.mapWithKey . indexed
