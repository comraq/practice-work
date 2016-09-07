module Sorting where

import Data.List (sort)
import Control.Parallel (par, pseq)

{-
 - Normal and Weak Head Normal Forms:
 -
 - The 'seq' function evaluates an expression to "Weak Head Normal Form (WHNF)".
 - The evaluation stops once it reaches the outermost constructor or lambda
 - abstraction(the "head"). This is distinct from "Normal Form (NF)", in
 - which an expression is completely evaluated.
 -
 - @link - http://stackoverflow.com/questions/6872898/haskell-what-is-weak-head-normal-form
 -}

qsort :: Ord a => [a] -> [a]
qsort (x:xs) = lesser ++ x:greater
  where lesser  = qsort [y | y <- xs, y <  x]
        greater = qsort [y | y <- xs, y >= x]
qsort _      = []

{-
 - 'pseq' is similar to 'seq' in the sense that it evaluates its left
 - expression to "WHNF" before returning the value of the expression on the
 - right. Unlike 'seq', 'pseq' also gurantees that its left argument will be
 - evaluated before its right argument.
 -
 - 'par' does not actually promise to evaluate an expression in parallel
 - with another. Instead, at runtime it decides whether an expression is too
 - cheap to be worth evaluating in parallel or not. For instance, if all
 - cores are busy, forcifully introducing a new thread to run in parallel is
 - not the most intelligent choice.
 -}
parQSort :: Ord a => [a] -> [a]
parQSort (x:xs) = force greater `par` (force lesser `pseq`
                                       (lesser ++ x:greater))
  where lesser  = parQSort [y | y <- xs, y <  x]
        greater = parQSort [y | y <- xs, y >= x]
parQSort _      = []

{-
 - Since 'sillyQSort' only sorts the 'lesser' and 'greater' sublists to
 - "WHNF", the work done in parallel only evaluates the first element of
 - each sublist as the outer most expression evaluates to the list
 - constructor! As a result, all other elements in the list remains
 - unevaluated and thus not much useful work is done in parallel.
 -
 - 'sillyQSort' ends up being nearly sequential!
 -}
sillyQSort :: Ord a => [a] -> [a]
sillyQSort (x:xs) = greater `par` (lesser `pseq`
                                   (lesser ++ x:greater))
  where lesser  = sillyQSort [y | y <- xs, y <  x]
        greater = sillyQSort [y | y <- xs, y >= x]
sillyQSort _      = []

{-
 - Pattern matching in 'go' forces the evaluation of the list constructor
 - (:). The usage of 'pseq' forces the evaluation of individual elements
 -}
force :: [a] -> ()
force xs = go xs `pseq` ()
  where go (_:xs) = go xs
        go []     = 1

seqQSort :: Ord a => [a] -> [a]
seqQSort (x:xs) = lesser `pseq` (greater `pseq`
                                 (lesser ++ x:greater))
  where lesser  = seqQSort [y | y <- xs, y <  x]
        greater = seqQSort [y | y <- xs, y >= x]
seqQSort _      = []

parQSort2 :: Ord a => Int -> [a] -> [a]
parQSort2 d list@(x:xs)
  | d <= 0    = sort list
  | otherwise = force greater `par` (force lesser `pseq`
                                     (lesser ++ x:greater))
      where lesser  = parQSort2 d' [y | y <- xs, y <  x]
            greater = parQSort2 d' [y | y <- xs, y >= x]
            d'      = d - 1
parQSort2 _ _           = []
