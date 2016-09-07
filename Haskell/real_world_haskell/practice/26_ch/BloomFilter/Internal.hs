module BloomFilter.Internal
  ( Bloom(..)
  , MutBloom(..)
  ) where

{-
 - A Bloom filter is a set-like data structure that is highly efficient in
 - its use of space. It only supports two operations: insertion and
 - membership querying. Unlike a normal set data structure, a Bloom filter
 - can give incorrect answers. If we query it to see whether an element that
 - we have inserted is present, it will answer affirmatively. If we query
 - for an element that we have not inserted, it might incorrectly claim that
 - the element is present.
 -}

{-
 - Applications:
 - Bloom filters are applicable to situations where a low rate of false
 - positives is tolerable. For instance, the job of a network traffic shaper
 - is to throttle bulk transfers (ex: BitTorrent) so that interactive
 - sessions (such as ssh sessions or games) see good response times. A
 - traffic shaper might use a Bloom filter to determine whether a packet
 - belonging to a particular session is bulk or interactive. If it
 - misidentifies one in ten thousand bulk packets as interactive and fails
 - to throttle it, nobody will notice.
 -}

{-
 - Space efficiency:
 - If we want to build a spell checker, and have a dictionary of half a
 - million words, a set data structure might consume 20MB of space. A Bloom
 - filter, in contrast, would consume about 0.5M, at the cost of missing
 - perhaps 1% of misspelled words.
 -}

{-
 - Internal Structure:
 - A simple data structure/implementation consisting of a bit array and a
 - handful of hash functions. We will use "k" for the number of hash
 - functions. If we want to insert a value into the Bloom filter, we compute
 - "k" hashes of the value, and turn on those bits in the bit array. If we
 - want to see whether a value is present, we compute "k" hashes, and check
 - all of those bits in the array to see if they are turned on.
 -}

{-
 - Example:
 -
 - > Inserting the strings "foo" and "bar" into a Bloom filter that is 8 bits
 -   wide, we have two hash functions:
 -
 -   1) Compute the two hashes of "foo", get the values 1 and 6
 -   2) Set bits 1 and 6 in the bit array
 -   3) Compute the two hashes of "bar" get the values 6 and 3
 -   4) Set bits 6 and 3 in the bit array
 -
 -   - Note: Since both "foo" and "bar" results in setting the bit 6, we do
 -           not differentiate between which element had set which bit and thus
 -           cannot safely remove an element from the Bloom filter
 -
 - > Query the Bloom filter for the values "quux" and "baz":
 -
 -   1) Compute the two hashes of "quux", get the values 4 and 0
 -   2) Check bit 4 in the bit array. If not set, then "quux" is not
 -      present, short circuit, no need to check bit 0
 -   3) Compute the two hashes of "baz", get the values 1 and 3
 -   4) Check bit 1 in the bit array. If set, check bit 3. If both are set,
 -      respond that "baz" is present (even though other elements may have
 -      trigged the setting of bits 1 and/or 3)
 -}

import Data.Array.ST (STUArray)
import Data.Array.Unboxed (UArray)
import Data.Word (Word32)

{-
 - Haskell Values:
 -
 - Normal Haskell types can either be fully evaluated, an unevaluated
 - thunk or "bottom" (where "bottom" is a placeholder for a computation that
 - does not succeed). "bottom" is a computation that can take many forms,
 - such as infinite loop, 'error' or 'undefined'.
 -
 - Any Haskell type that can contain "bottom" is considered as "lifted".
 - Note that all normal Haskell types are "lifted". (ie: we can always
 - return 'error "some string"' or 'undefined' in any expression)
 -
 - This ability to store thunks or "bottom" comes with a performance
 - cost, as it adds an extra layer of indirection. Consider 'Word32',
 - a value of this type should be 32 bits wide. On a 32-bit system,
 - there is no way to directly encode bottom within 32 bits, thus the
 - runtim esystem has to maintain and chec some extra data to track
 - whether the value is bottom or not.
 -
 - On the other hand, an unboxed value does away with this indirection, and
 - thus gains performance. However, it sacrifices the ability to represent a
 - thunk or "bottom".
 -
 - Note: 'UArray' contains "unboxed" values.
 -}

{-
 - Boxing and Lifting:
 -
 - The "dual" of unboxed type is a "boxed type" (which uses the indirection
 - mentioned previously). All "lifted" types are "boxed", but a few
 - low-leveled "boxed" types are not "lifted". For instance, GHC's runtime
 - system has a low-level array type for which is uses boxing (ie: it
 - maintains a pointer to the array). If it has a reference to such an
 - array, it knows that the array must exist, so it does not need to account
 - for the possibilty of "bottom". This array type is thus "boxed", but not
 - "lifted". "Boxed" but "unlifted" types only show up at the lowest level
 - of runtime hacking, not in high level application programs.
 -}

data Bloom a = B {
  blmHash  :: a -> [Word32]
, blmArray :: UArray Word32 Bool
}

data MutBloom s a = MB {
  mutHash  :: a -> [Word32]
, mutArray :: STUArray s Word32 Bool -- A mutable unboxed array
}
