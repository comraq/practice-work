module Strat where

import Control.Parallel (pseq, par)

type Done = ()
type Strategy a = a -> Done

{-
 - An evaluation strategy performs no computation, as it simply ensures that
 - a value is evaluated to some extent.
 -}
r0 :: Strategy a
r0 _ = ()

-- Acronym for "Reduce to Weak Head Normal Form"
rwhnf :: Strategy a
rwhnf x = x `pseq` ()

-- Acronym for "Normal Form Data"
class NFData a where
  rnf :: Strategy a
  rnf = rwhnf

instance NFData Char
instance NFData Int

{-
 - Any custom implementation of 'NFData' must have an 'rnf' implementation
 - for every constructor and apply 'rnf' to every field of each constructor.
 -}
instance NFData a => NFData (Maybe a) where
  rnf Nothing  = ()
  rnf (Just x) = rnf x

parList :: Strategy a -> Strategy [a]
parList strat []     = ()
parList strat (x:xs) = strat x `par` parList strat xs

parMap :: Strategy b -> (a -> b) -> [a] -> [b]
parMap strat f xs = map f xs `using` parList strat

using :: a -> Strategy a -> a
using x s = s x `pseq` x

vectorSum' :: (NFData a, Num a) => [a] -> [a] -> [a]
vectorSum' = parZipWith rnf (+)

parZipWith :: Strategy b -> (a -> a -> b) -> [a] -> [a] -> [b]
parZipWith strat f xs ys = zipWith f xs ys `using` parList strat
