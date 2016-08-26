{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Test.QuickCheck

import qualified Sudoku as S

{-
 - Constrain the domain of Int to between 3 to 5 for Sudoku Block Sizes
 -
 - @link - https://www.reddit.com/r/haskellquestions/comments/rbctw/restricting_quickcheck_int_input/
 -}

newtype SizeInt = SizeInt Int
  deriving (Eq, Ord, Show, Num, Integral, Real, Enum)

instance Arbitrary SizeInt where
  arbitrary = fmap SizeInt (choose (3, 5) :: Gen Int)

prop_getValues :: SizeInt -> Bool
prop_getValues (SizeInt n) = (length $ S.getValues n) == n * n
