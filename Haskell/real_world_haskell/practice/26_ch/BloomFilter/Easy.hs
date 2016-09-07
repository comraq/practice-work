module BloomFilter.Easy
  ( suggestSizing
  , sizings
  , easyList

  -- re-export useful names from BloomFilter
  , B.Bloom
  , B.length
  , B.elem
  , B.notElem
  ) where

import BloomFilter.Hash (Hashable, doubleHash)
import Data.List (genericLength)
import Data.Maybe (catMaybes)
import Data.Word (Word32)
import qualified BloomFilter as B

{-
 - A user friendly function to create a Bloom filter.
 - It leaves the responsibility of hasing values to the 'Hashable'
 - typeclass, letting users configure the parameters of the Bloom filter in
 - terms that are easier to understand (ie: the tolerable rate of false positives)
 -
 - Result is wrapped in and 'Either' monad because the function may fail if
 - the list is too long.
 -}
easyList :: Hashable a
         => Double                    -- false positive rate between 0 and 1
         -> [a]                       -- values to populate the filter with
         -> Either String (B.Bloom a)
easyList errRate values = case suggestSizing (genericLength values) errRate of
    Left err             -> Left err
    Right (bits, hashes) -> Right filt
      where filt = B.fromList (doubleHash hashes) bits values

{-
 - 'suggestSizing' estimates the best combination of filter size and number
 - of hashes to compute, based on the desired tolerable false positive rate
 - and the maximum number of elements expected to be contained in the filter
 -}
suggestSizing :: Integer                     -- expected maximum capacity
              -> Double                      -- desired false positive rate
              -> Either String (Word32, Int) -- (filter size, num of hashes)
suggestSizing capacity errRate
    | capacity <= 0                 = Left "capacity too small"
    | errRate  <= 0 || errRate >= 1 = Left "invalid error rate"
    | null saneSizes                = Left "capacity too large"
    | otherwise                     = Right $ minimum saneSizes

  where saneSizes = catMaybes . map sanitize $ sizings capacity errRate
        sanitize (bits, hashes)
          | bits > maxWord32 - 1 = Nothing
          | otherwise             = Just (ceiling bits, truncate hashes)
          where maxWord32 = fromIntegral (maxBound :: Word32)

-- Returns (array size, and hash count)
sizings :: Integer -> Double -> [(Double, Double)]
sizings capacity errRate =
    [((-k) * cap / log (1 - (errRate ** (1 / k))), k) | k <- [1..50]]
  where cap = fromIntegral capacity
