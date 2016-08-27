import System.IO
import Data.Char (toUpper)
import Control.Arrow ((***))

main1 :: IO ()
main1 = do
  inH <- openFile "input.txt" ReadMode
  outH <- openFile "output.txt" WriteMode
  inStr <- hGetContents inH
  let result = processData inStr
  hPutStr outH result
  hClose inH
  hClose outH

processData :: String -> String
processData = map toUpper

{-
 - hGetContents :: Handle -> IO String
 - - in eager languages, this may be a bad idea as this kind of function
 -   typically buffers the entire string contents into memory
 -
 - - in Haskell, the String value in the IO String is evaluated lazily. (ie:
 -   when 'hGetContents' is called, no data is actually read) Only when
 -   characters are required to be processed from the list will the
 -   necessary number of characters are read.
 - - as elements of String are no longer references, the garbage collector
 -   will free the memory, and thus the Handle can point to a arbitrarily
 -   large file, as long as the list processing does not require storing all
 -   of the contents into a single location at one, 'hGetContents' will not
 -   crash the program due to lack of RAM
 -   - ex: in 'main', if the 'inStr' binding is used further below the call
 -         to 'processData', then the above mentioned memory efficiency
 -         would have been lost as the later usage of 'inStr' would prevent
 -         the garbage collector from freeing up the memory
 -
 - Note: Since Haskell is lazy, it is possible to close the Handle passed to
 -       'hGetContents' before even consuming the actual contents!
 -       If so, then the actual content data will be missed as the Handle is
 -       already closed. Thus ensure consuming the contents before calling
 -       'hClose'.
 -}

main2 :: IO ()
main2 = do
  inH <- openFile "input.txt" ReadMode
  outH <- openFile "output.txt" WriteMode
  inStr <- hGetContents inH
  hPutStr outH (map toUpper inStr)
  hClose inH
  hClose outH

-- readFile and writeFile are just shorthand functions for:
-- - (>>= hGetContents) . flip openFile ReadMode
-- - curry $ uncurry (>>=) . ((flip openFile WriteMode) *** (flip hPutStr))
--
-- Though these do not call 'hClose' on the Handle? (Automatically closed
-- when the underlying String is consumed?)

readFile' :: FilePath -> IO String
readFile' = (>>= hGetContents) . flip openFile ReadMode

writeFile' :: FilePath -> String -> IO ()
writeFile' = curry $ uncurry (>>=) . (flip openFile WriteMode *** flip hPutStr)

main3 :: IO ()
main3 = do
  inStr <- readFile' "input.txt"
  writeFile' "output.txt" (map toUpper inStr)

main :: IO ()
main = interact (map toUpper)

main5 :: IO ()
main5 = interact (map toUpper . (++) "Your data, in uppercase, is:\n\n")

main6 :: IO ()
main6 = interact ((++) "Your data, in uppercase, is:\n\n" .  map toUpper)

main7 :: IO ()
main7 = interact (unlines . filter (elem 'a') . lines)
