import System.IO
import Data.Char (toUpper)

main :: IO ()
main = do
  inH <- openFile "input.txt" ReadMode
  outH <- openFile "output.txt" WriteMode
  mainloop inH outH
  hClose inH
  hClose outH

mainloop :: Handle -> Handle -> IO ()
mainloop inH outH = do
  inEOF <- hIsEOF inH
  if inEOF
  then return ()
  else do
    inStr <- hGetLine inH
    hPutStrLn outH (map toUpper inStr)
    mainloop inH outH

{-
 - Note: Non-"h" IO functions are usually a simple shorthand mapping the implicit
 -       file handle to stdin, stdout and etc...
 -       ex: getLine  = hGetLine  stdin
 -           putStrLn = hPutStrLn stdout
 -           print    = hPrint    stdout
 -}
