module Utils
  ( module Utils
  , module Control.Arrow
  ) where

import Control.Arrow

(.*) :: (b -> c) -> (a0 -> a1 -> b) -> a0 -> a1 -> c
(.*) = (.) . (.)

(?>>) :: Bool -> a -> a -> a
(?>>) b x y = if b then x else y

(<<?) :: a -> a -> Bool -> a
(<<?) x y b = if b then x else y
