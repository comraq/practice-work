{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign
import Foreign.C.Types

{-
 - The following defines a new Haskell function 'c_sin', whose concrete
 - implementation is in C, via its 'sin' function.
 -
 - When 'c_sin' is called, a call the actual C function 'sin' will be made
 - (using the standard C calling convention, indicated by the 'ccall'
 - keyword). The Haskell runtime passes control to C, which returns its
 - results back to Haskell. The result is then wrapped up as a Haskell value
 - of typoe 'CDouble'.
 -
 - Note that the actual C 'sin' function has the following signature:
 -   'double sin(double x)'
 -}
foreign import ccall "math.h sin"
  c_sin :: CDouble -> CDouble

{-
 - Note that code with side effects are more common in imperative languages.
 - Thus we must encapsulate the import Haskell function with the type
 - wrapped in the IO Monad (ex: 'IO CDouble')
 -}
fastsin :: Double -> Double
fastsin x = realToFrac . c_sin . realToFrac $ x

main :: IO ()
main = mapM_ (print . fastsin) [0/10, 1/10 .. 10/10]

