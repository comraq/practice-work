import Control.Arrow

f1 :: (a -> c) -> (b -> d) -> a -> b -> (c, d)
f1 = curry $ curry . uncurry (***)

on :: (a -> a -> b) -> (c -> a) -> c -> c -> b
f `on` g = \x y -> f (g x) (g y)

on' :: (a -> a -> b) -> (c -> a) -> c -> c -> b
f `on'` g = curry $ uncurry f . (g *** g)

dot :: (b -> c) -> (a0 -> a1 -> b) -> a0 -> a1 -> c
dot = (.) . (.)
