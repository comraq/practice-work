{-
 - The 'System.IO.Error' module (different from 'Control.Exception') defines
 - two functions 'catch' and 'tryIOError'.
 -
 - Unlike the 'Control.Exception' functions, these functions will only trap
 - 'IO' erros, and will pass all other exceptions through uncaught. In
 - Haskell, 'IO' error all have the type 'IOError', which is defined as the
 - same as 'IOException'.
 -}

{-
 - Note that both 'System.IO.Error' and 'Control.Exception' define functions
 - with the same names, and thus one or the other module should be imported
 - as 'qualified' or hidden.
 -
 - Note: 'Prelude' exports 'System.IO.Error's version of 'catch', not the
 -       version provided by 'Control.Exception'. Remember that this
 -       exported version can only catch errors of type 'IOError', while
 -       'catch' from 'Control.Exception' can catch all exceptions.
 -       ie: "The 'catch' in 'Control.Exception' is almost always the one
 -           preferred, but not the one exported by 'Prelude' by default.
 -}

import System.IO
import System.IO.Error
import Data.Char (toUpper)

main :: IO ()
main = do
  inh  <- openFile "input.txt"  ReadMode
  outh <- openFile "output.txt" WriteMode
  mainloop inh outh
  hClose inh
  hClose outh

{-
 - This version of 'mainloop' uses 'tryIOError' to check whether 'hGetLine' threw
 - an 'IOError'. If it did throuh an EOFError, we catch and exit the loop.
 - Otherwise, we rethrow the error with 'ioError'.
 -}
mainloop :: Handle -> Handle -> IO ()
mainloop inh outh = do
  input <- tryIOError $ hGetLine inh
  case input of
    Left e       -> if isEOFError e
                    then return ()
                    else ioError e
    Right inpStr -> do
      hPutStrLn outh . map toUpper $ inpStr
      mainloop inh outh
