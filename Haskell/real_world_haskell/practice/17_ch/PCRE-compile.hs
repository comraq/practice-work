{-# LANGUAGE ForeignFunctionInterface #-}

import qualified Data.ByteString.Char8 as B
import System.IO.Unsafe (unsafePerformIO)

{-
 - Use a pointer to '()' to represent unknown foreign data
 -
 - Note: We will never dereference pointers of this type
 -}
type PCRE = ()

foreign import ccall unsafe "pcre.h pcre_compile"
  c_pcre_compile :: CString
                 -> PCREOption
                 -> Ptr CString
                 -> Ptr CInt
                 -> Ptr Word8
                 -> IO (Ptr PCRE)

{-
 - Note that 'unsafe' indicates that the C code imported cannot call back
 - into Haskell, where as default imported 'safe' C code can.
 -}

{-
 - We can define a unique type, distinct from unit '()' via the following:
 -   data PCRE
 -
 - This is known as the nullary data type, a type for which no data can be
 - constructed.
 -
 - Note: This requries the 'EmptyDataDecls' LANGUAGE extension.
 - Note: "bottom"/'undefined' are still valid values of this type.
 -}

{-
 - To achieve the same result without using the 'EmptyDataDecls' LANGUAGE
 - extension, we can define a recursive 'newtype' like so:
 -   newtype PCRE = PCRE (Ptr PCRE)
 -}

{-
 - Hides the manually managed 'Ptr PCRE' type inside an automatically
 - managed data structrue
 -
 - 'ForeignPtr' is used to manage the underlying 'PCRE' data allocated in C.
 -
 - 'ByteString' is the string representation of the regular expression
 -}
data Regex = Regex !(ForeignPtr PCRE)
                   !ByteString
  deriving (Eq, Ord, Show)

{-
 - The compilation from regex string to compiled C regex pattern should be
 - referentially transparent. Regardless of the memory location pointed to
 - by the pointers which the C library returns for functionally identical
 - expressions.
 -
 - Regular expression input can still fail, and thus we should denote
 - failure within an error value, 'Either'.
 -}
compile :: B.ByteString -> [PCREOption] -> Either String Regex
compile str flags = unsafePerformIO $
  useAsCString str $ \pattern -> do
    alloca $ \errptr       -> do
    alloca $ \erroffset    -> do
      pcre_ptr <- c_pcre_compile pattern (combineOptions flags) errptr erroffset nullPtr
      if pcre_ptr == nullPtr
      then do
        err <- peekCString =<< peekerrptr
        return $ Left err
      else do
        reg <- newForeignPtr finalizerFree pcre_ptr -- release with free() in C
        return . Right $ Regex reg str

{-
 - unsafePerformIO :: IO a -> a
 -
 - This function takes an IO value and converts it to a pure value!
 - This function essentially takes all the dangerous effects out of the
 - safety of the IO Monad into any regular Haskell program!
 -
 - Potential Dangers:
 - - break optimizations
 - - modify arbitrary locations in memory
 - - remove files on the user's machine
 - - "launch nuclear missles from an innocent Fibonacci sequence"
 -}
