module Utils
  ( module Utils
  , module Control.Arrow
  , module Data.Tuple
  , module Data.Profunctor
  ) where

import Control.Arrow
import Data.Tuple
import Data.Profunctor

{-
 - Note: (<<<) === (.)
 -       (>>>) === flip (.)
 -}

infixr 9 .*
infixr 9 .**

(.*) :: (b -> c) -> (a0 -> a1 -> b) -> a0 -> a1 -> c
(.*) = (.) . (.)

(.**) :: (b -> c) -> (a0 -> a1 -> a2 -> b) -> a0 -> a1 -> a2 -> c
(.**) = (.*) . (.)

infixr 1 *.
infixr 1 **.

(*.) :: (a0 -> a1 -> b) -> (b -> c) -> a0 -> a1 -> c
(*.) = flip (.*)

(**.) :: (a0 -> a1 -> a2 -> b) -> (b -> c) -> a0 -> a1 -> a2 -> c
(**.) = flip (.**)

(?>>) :: Bool -> a -> a -> a
(?>>) b x y = if b then x else y

(<<?) :: a -> a -> Bool -> a
(<<?) x y b = if b then x else y

dup :: a -> (a, a)
dup = id &&& id

app' :: (b -> c) -> b -> c
app' = curry app
