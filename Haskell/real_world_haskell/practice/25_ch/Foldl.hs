module Foldl where

{-
 - Use 'seq' to reduce the accumulated state at each step, to "WHNF" (weak head normal form)
 -}
foldl'' :: (a -> b -> a) -> a -> [b] -> a
foldl'' f z xs = lgo z xs
  where lgo z []     = z
        lgo z (x:xs) = let z' = f z x
                       in z' `seq` lgo z' xs
