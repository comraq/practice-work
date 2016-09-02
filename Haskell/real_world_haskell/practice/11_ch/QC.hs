module QC where

import Utils

import Prettify2
import Test.QuickCheck
import Data.List (intersperse)

{-
 - Example Definition of the Char Arbitrary instance:
 -
 - instance Arbitrary Char where
 -   arbitrary = elements (['A'..'Z'] ++ ['a'..'z'] ++ " ~!@#$%^&*()")
 -}

instance Arbitrary Doc where
  arbitrary = do
    n <- choose (1, 6) :: Gen Int
    case n of
      1 -> return Empty

      2 -> do x <- arbitrary
              return $ Char x

      3 -> do x <- arbitrary
              return $ Text x

      4 -> return Line

      5 -> do x <- arbitrary
              y <- arbitrary
              return $ Concat x y

      6 -> do x <- arbitrary
              y <- arbitrary
              return $ Union x y

{-
 - Alternation definiton of 'arbitrary' for the Doc Arbitrary instance:
 -   arbitrary =
 -     oneof [ return Empty
 -           , liftM  Char   arbitrary
 -           , liftM  Text   arbitrary
 -           , return Line
 -           , liftM2 Concat arbitrary arbitrary
 -           , liftM2 Union  arbitrary arbitrary ]
 -}

prop_empty_id :: Doc -> Bool
prop_empty_id x = empty <> x == x
               && x <> empty == x

prop_char :: Char -> Bool
-- prop_char c = char c == Char c
prop_char = (char &&& Char) >>> uncurry (==)

prop_text :: String -> Bool
-- prop_text s = text s == if null s then Empty else Text s
prop_text = (text &&& checkIfEmpty) >>> uncurry (==)
  where emptyOrText = (const Empty &&& Text) >>> uncurry (<<?)
        checkIfEmpty = (emptyOrText &&& null) >>> app

prop_line :: Bool
prop_line = line == Line

prop_double :: Double -> Bool
-- prop_double d = double d == (text . show) d
prop_double = (double &&& (text . show)) >>> uncurry (==)

prop_hcat :: [Doc] -> Bool
-- prop_hcat xs = hcat xs == glue xs
prop_hcat = (hcat &&& glue) >>> uncurry (==)
  where glue []     = empty
        glue (d:ds) = d <> glue ds

prop_punctuate :: Doc -> [Doc] -> Bool
-- prop_punctuate s xs = punctuate s xs == intersperse s xs
prop_punctuate = (punctuate &&& intersperse)
  >>> uncurry (&&&)
  *. uncurry (==)

prop_punctuate' :: Doc -> [Doc] -> Bool
prop_punctuate' s xs = punctuate s xs == combine (intersperse s xs)
  where
    combine []           = []
    combine [x]          = [x]

    combine (x:Empty:ys) = x: combine ys
    combine (Empty:y:ys) = y: combine ys
    combine (x:y:ys)     = x `Concat` y : combine ys

prop_mempty_id :: Doc -> Bool
-- prop_mempty_id x = mempty `mappend` x == x
--                 && x `mappend` mempty == (x :: Doc)
prop_mempty_id = (emptyAppendVal &&& valAppendEmpty) >>> uncurry (&&)
  where emptyAppendVal = (mappend mempty &&& (==)) >>> swap >>> app
        valAppendEmpty = ((==) &&& mappend mempty) >>> app
