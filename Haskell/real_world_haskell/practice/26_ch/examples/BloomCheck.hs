module Main where

{-
 - Run ghci with the following options (to correctly load the C FFI library):
 - > ghci -L./cbits -llookup3
 -}
import BloomFilter.Hash (Hashable)
import Data.Word (Word8, Word32)
import System.Random (Random(..), RandomGen)
import Test.QuickCheck
import qualified BloomFilter.Easy as B
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy

handyCheck :: Testable a => Int -> a -> IO ()
handyCheck limit = quickCheckWith $ stdArgs { maxSuccess = limit }

{-
 - We will specifiy the range of valid 'Double' to generate (as we can only
 - accept values of range (0, 1) (exclusive)
 -}
falsePositive :: Gen Double
falsePositive = choose (epsilon, 1 - epsilon)
  where epsilon = 1e-6

(=~>) :: Either a b -> (b -> Bool) -> Bool
k =~> f = either (const True) f k

{-
 - Ensure that adding a value then testing its membership should always be
 - true
 -}
prop_one_present :: Hashable a => t -> a -> Property
prop_one_present _ elt = forAll falsePositive $ \errRate ->
                         B.easyList errRate [elt] =~> \filt ->
                         elt `B.elem` filt

prop_all_present :: Hashable a => t -> [a] -> Property
prop_all_present _ xs = forAll falsePositive $ \errRate ->
  B.easyList errRate xs =~> \filt ->
    all (`B.elem` filt) xs

{-
 - By default, there are no Arbitrary and CoArbitrary implementations for
 - both lazy and strict 'ByteString' types. Thus, we must provide our own
 - implementations for the Arbitrary and CoArbitrary typeclasses.
 -}

instance Arbitrary Lazy.ByteString where
  arbitrary = Lazy.pack <$> arbitrary

instance CoArbitrary Lazy.ByteString where
  coarbitrary = coarbitrary . Lazy.unpack

instance Arbitrary Strict.ByteString where
  arbitrary = Strict.pack <$> arbitrary

instance CoArbitrary Strict.ByteString where
  coarbitrary = coarbitrary . Strict.unpack

{-
 - Example definitions of Arbitrary and CoArbitrary implementations for
 - the types 'Word8' and 'Word32'
 -
 - integralCoarbitrary n = variant $
 -   if m >= 0
 -   then 2 * m
 -   else 2 * (-m) + 1
 -
 - integralRandomR (a, b) g = case randomR (c, d) g of
 -     (x, h) -> (fromIntegral x, h)
 -   where (c, d) = ( fromIntegral a :: Integer
 -                  , fromIntegral b :: Integer)
 -
 - instance Random Word8 where
 -   randomR = integralRandomR
 -   random  = randomR (minBound, maxBound)
 -
 - instance Arbitrary Word8 where
 -   arbitrary = choose (minBound, maxBound)
 -
 - instance Coarbitrary Word8 where
 -   coarbitrary = integralCoarbitrary
 -
 - instance Random Word32 where
 -   randomR = integralRandomR
 -   random  = randomR (minBound, maxBound)
 -
 - instance Arbitrary Word32 where
 -   arbitrary = choose (minBound, maxBound)
 -
 - instance Coarbitrary Word32 where
 -   coarbitrary = integralCoarbitrary
 -}

{-
 - Testing 'suggestSizing':
 - - To do so, we must vary both the dersired false positive rate and
 - expected capacity
 -}

-- An attempt ignoring reasonable capacity
prop_suggest_try1 :: Property
prop_suggest_try1 = forAll falsePositive $ \errRate ->
  forAll (choose (1, maxBound :: Word32)) $ \cap ->
    case B.suggestSizing (fromIntegral cap) errRate of
      Left err             -> False
      Right (bits, hashes) -> bits > 0 && bits < maxBound && hashes > 0

{-
 - Since it is difficult to predict which combination of input causes
 - 'suggestSizing' to return 'Left "capacity too large"', we must resort to
 - eliminating sizes and false positive rates before running the tests
 -
 - Notice that this attemp discards too any input combinations due to the
 - filtering from (==>) that occurs after an arbitrary input is generated.
 -}
prop_suggest_try2 :: Property
prop_suggest_try2 = forAll falsePositive $ \errRate ->
    forAll (choose (1, fromIntegral maxWord32)) $ \cap ->
      let bestSize = fst . minimum $ B.sizings cap errRate
      in  bestSize < fromIntegral maxWord32 ==>
          either (const False) sane $ B.suggestSizing cap errRate

  where sane (bits, hashes) = bits > 0 && bits < maxBound && hashes > 0
        maxWord32           = maxBound :: Word32

prop_suggestions_sane :: Property
prop_suggestions_sane = forAll falsePositive $ \errRate ->
    forAll (choose (1, fromIntegral maxWord32 `div` 8)) $ \cap ->
      let size = fst . minimum $ B.sizings cap errRate
      in  size < fromIntegral maxWord32 ==>
          either (const False) sane $ B.suggestSizing cap errRate

  where sane (bits, hashes) = bits > 0 && bits < maxBound && hashes > 0
        maxWord32           = maxBound :: Word32
