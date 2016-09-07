{-# LANGUAGE FlexibleInstances
           , UndecidableInstances
           , ForeignFunctionInterface
  #-}

module BloomFilter.Hash
  ( Hashable(..)
  , hash
  , doubleHash
  ) where

{-
 - Since Bloom filter depends on fast, high-quality hashes for good
 - performance and low false positive rates, we need a good general purpose
 - hash function that can satisfy this requirement.
 -}

import Prelude hiding (words)
import Data.Bits ((.&.), shiftR)
import Foreign.Marshal.Array (withArrayLen)
import Control.Monad (foldM)
import Data.Word (Word32, Word64)
import Foreign.C.Types (CSize(..))
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable, peek, sizeOf)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import System.IO.Unsafe (unsafePerformIO)

foreign import ccall unsafe "lookup3.h hashword2" hashWord2
  :: Ptr Word32 -> CSize -> Ptr Word32 -> Ptr Word32 -> IO ()

foreign import ccall unsafe "lookup3.h hashlittle2" hashLittle2
  :: Ptr a -> CSize -> Ptr Word32 -> Ptr Word32 -> IO ()

{-
 - The 'with' function allocates room for the salt on the C stack, and
 - stores the current value in there so 'sp' in the lambda function is a
 - 'Ptr Word64'.
 -
 - The pointers 'p1' and 'p2' are 'Ptr Word32' as 'p1' points at the lower
 - word of 'sp' and 'p2' at the higher word. Thus, splitting 'p1' and 'p2'
 - from 1 salt to 2 salts.
 -
 -}
hashIO :: Ptr a
       -> CSize
       -> Word64
       -> IO Word64
hashIO ptr bytes salt = with (fromIntegral salt) $ \sp -> do
    let p1 = castPtr sp
        p2 = castPtr sp `plusPtr` 4
    go p1 p2
    peek sp

  where go p1 p2
          | bytes .&. 3 == 0 = hashWord2 (castPtr ptr) words p1 p2
          | otherwise        = hashLittle2 ptr bytes p1 p2
        words    = bytes `div` 4

class Hashable a where
  hashWithSalt :: Word64
               -> a        -- ^ salt
               -> Word64   -- ^ value to hash

hash :: Hashable a => a -> Word64
hash = hashWithSalt 0x006fc397cf62f64d3

-- Boilerplate for hashing basic/common types
hashStorable :: Storable a => Word64 -> a -> Word64
hashStorable salt k = unsafePerformIO . with k $ \ptr ->
  hashIO ptr (fromIntegral (sizeOf k)) salt

{-
 - instance Hashable Char   where hashSalt = hashStorable
 - instance Hashable Int    where hashSalt = hashStorable
 - instance Hashable Double where hashSalt = hashStorable
 - instance Storable a => Hashable a where
 -   hashWithSalt = hashStorable
 -}

hashList :: Storable a => Word64 -> [a] -> IO Word64
hashList salt xs = withArrayLen xs $ \len ptr ->
    hashIO ptr (fromIntegral (len * sizeOf x)) salt
  where x = head xs

-- instance Storable a => Hashable [a] where
--   hashWithSalt salt xs = unsafePerformIO $ hashList salt xs

hash2 :: Hashable a => a -> Word64 -> Word64
hash2 k salt = hashWithSalt salt k

instance (Hashable a, Hashable b) => Hashable (a, b) where
  hashWithSalt salt (a, b) = hash2 b . hash2 a $ salt

instance (Hashable a, Hashable b, Hashable c) => Hashable (a, b, c) where
  hashWithSalt salt (a, b, c) = hash2 c . hash2 b . hash2 a $ salt

hashByteString :: Word64 -> Strict.ByteString -> IO Word64
hashByteString salt bs = Strict.useAsCStringLen bs $ \(ptr, len) ->
  hashIO ptr (fromIntegral len) salt

instance Hashable Strict.ByteString where
  hashWithSalt salt bs = unsafePerformIO $ hashByteString salt bs

rechunk :: Lazy.ByteString -> [Strict.ByteString]
rechunk s
  | Lazy.null s = []
  | otherwise   = let (pre, suf) = Lazy.splitAt chunkSize s
                  in repack pre : rechunk suf

  where repack    = Strict.concat . Lazy.toChunks
        chunkSize = 64 * 1024

instance Hashable Lazy.ByteString where
  hashWithSalt salt bs = unsafePerformIO .
                         foldM hashByteString salt $ rechunk bs

{-
 - Since we need more than two hashes to make effective use of Bloom filter,
 - we use "double hashing" to combine two values computed by the hash
 - functions, yielding more hashes.
 -
 - Although the resulting hash is not as unique, it is far cheapter than
 - computing many distinct hashes.
 -}
doubleHash :: Hashable a => Int -> a -> [Word32]
doubleHash numHashes value = [h1 + h2 * i | i <- [0..num]]
  where h   = hashWithSalt 0x9150a946c4a89663 value
        h1  = fromIntegral (h `shiftR` 32) .&. maxBound
        h2  = fromIntegral h
        num = fromIntegral numHashes
