module BloomFilter.Mutable
  ( MutBloom
  , elem
  , notElem
  , insert
  , length
  , new
  ) where

{-
 - The 'ST' Monad
 -
 - Unlike the 'State' monad, the 'ST' monad gives the ability to work safely
 - with mutable state as follows:
 - - "thaw" an immutable array to give a mutable array (ie: modify the
 -   mutable array in place)
 - - "freeze" a mutable array into a new immutable array
 - - "mutable references", which allows the implementation of data structure
 -   that can be modified after construction
 -
 - Note: Although the 'IO' monad also provide similar capabilities, values
 -       cannot be retreived back from the 'IO' monad. However, with the
 -       'ST' monad, the value can be extracted out via 'runST', much like
 -       typical other monads.
 -
 -       However, in order for 'runST' to be deterministic, some
 -       restrictions must be imposed on the 'ST' monad. For example, we
 -       cannot read or write files, create global variables, or fork
 -       threads. In addition, mutable references and arrays cannot be
 -       extracted from the 'ST' monad unless:
 -         > mutable array are re-frozen
 -         > mutable references cannot be extracted at all
 -}

import Control.Monad (liftM)
import Control.Monad.ST (ST)
import Data.Array.MArray (getBounds, newArray, readArray, writeArray)
import Data.Word (Word32)
import Prelude hiding (elem, length, notElem)

import BloomFilter.Internal (MutBloom(..))

new :: (a -> [Word32]) -> Word32 -> ST s (MutBloom s a)
new hash numBits = MB hash `liftM` newArray (0, numBits - 1) False

{-
 - Uses the bit array's own record of its bounds via 'getBounds'. Since our
 - array index is always from "0" to "length - 1", we take the upper bound
 - and increment by "1" to get the proper length (ie: succ . snd)
 -}
length :: MutBloom s a -> ST s Word32
length filt = (succ . snd) `liftM` getBounds (mutArray filt)

{-
 - Set the array bits when inserting. We use 'mod' to ensure that all of the
 - hashes stay within the bounds of our array, and isolate the code that
 - computes offsets into the bit array into the function 'indices'.
 -}
insert :: MutBloom s a -> a -> ST s ()
insert filt elt = indices filt elt >>=
                  mapM_ (\bit -> writeArray (mutArray filt) bit True)

indices :: MutBloom s a -> a -> ST s [Word32]
indices filt elt = do
  modulus <- length filt
  return $ map (`mod` modulus) (mutHash filt elt)

{-
 - 'elem':
 -   - Checks if all hashed bits are in the array
 -
 - 'notElem':
 -   - Logical negation of 'elem'
 -}
elem, notElem :: a -> MutBloom s a -> ST s Bool
elem elt filt = indices filt elt >>=
                allM (readArray $ mutArray filt)
notElem elt filt = not `liftM` elem elt filt

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM p (x:xs) = do
  ok <- p x
  if ok
    then allM p xs
    else return False
allM _ []     = return True
