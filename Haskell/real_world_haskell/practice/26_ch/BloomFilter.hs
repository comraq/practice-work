module BloomFilter
  ( Bloom
  , length
  , elem
  , notElem
  , fromList
  ) where

import BloomFilter.Internal
import BloomFilter.Mutable (insert, new)
import Data.Array.ST (runSTUArray)
import Data.Array.IArray ((!), bounds)
import Data.Word (Word32)
import Prelude hiding (elem, length, notElem)

length :: Bloom a -> Int
length = fromIntegral . len

len :: Bloom a -> Word32
len = succ . snd . bounds . blmArray

elem :: a -> Bloom a -> Bool
elt `elem` filt = all test (blmHash filt elt)
  where test hash = blmArray filt ! (hash `mod` len filt)

notElem :: a -> Bloom a -> Bool
elt `notElem` filt = not $ elt `elem` filt

{-
 - Note the use of 'runSTUArray', as it combines 'runST' to first freeze the
 - array within the 'ST' monad then returns an 'UArray'.
 -
 - The 'MArray' typeclass also provides a 'freeze' function that we could
 - use. However, 'runSTUArray' is both more convenient and efficient,  as
 - the efficiency lies in the fact that 'freeze' must copy the underlying
 - data from 'STUArray' to the new 'UArray', ensuring that no modifications
 - to the any reference to the old 'STUArray' can affect the new immutable
 - 'UArray'. However, 'runSTUArray' does not have this problem thanks to the
 - type system.
 -}
fromList :: (a -> [Word32]) -- family of hash functions
         -> Word32          -- number of bits in filter
         -> [a]             -- values to populate with
         -> Bloom a
fromList hash numBits values = B hash $ runSTUArray $ do
  mb <- new hash numBits
  mapM_ (insert mb) values
  return $ mutArray mb
