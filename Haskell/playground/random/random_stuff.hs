import Control.Arrow ((***))

f1 :: (a -> c) -> (b -> d) -> a -> b -> (c, d)
f1 = curry $ curry . uncurry (***)
