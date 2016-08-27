import System.IO
import System.Directory (getTemporaryDirectory, removeFile)
import Control.Exception (catch, finally, IOException)

main :: IO ()
main = withTempFile "some_temp_file.txt" myAction

{-
 - The guts of the program. Called with the path and handle of a temporary
 - file. When this function exits, that file will be closed and deleted
 - because myAction was called from withTempFile.
 -}

myAction :: FilePath -> Handle -> IO ()
myAction tempName tempH = do

  -- Start by displaying a greeting on the terminal
  putStrLn "Welcome to tempfile.hs"
  putStrLn $ "I have a temporary file at " ++ tempName

  -- Let's see what the initial position is
  pos <- hTell tempH
  putStrLn $ "My initial position is " ++ show pos

  -- Now, write some data to the temporary file
  let tempData = show [1..17]
  putStrLn $ "Writing one line containing " ++
             show (length tempData) ++ " bytes: " ++ tempData
  hPutStrLn tempH tempData

  -- Get our new position. This doesn't actually modify pos in memory, but
  -- makes the name "pos" correspond to a different value for the remainder
  -- of the "do" block (ie: no mutation/assignment, just a new binding)
  pos <- hTell tempH
  putStrLn $ "After writing, my new position is " ++ show pos

  -- Seek to the beginning of the file and display it
  putStrLn $ "The file content is: "
  hSeek tempH AbsoluteSeek 0

  -- hGetContents performsa a lazy read of the entire file
  c <- hGetContents tempH

  -- Copy the file byte-by-byte to stdout, followed by a '\n' char
  putStrLn c

  -- Let's also display it as a Haskell literal
  putStrLn $ "Which could be expressed as this Haskell literal:"
  print c



{-
 - This function takes two parameters:
 - 1) a filename pattern
 - 2) a function
 -
 - It will create a temporary file, and pass the name and Handle of the file
 - to the argument function
 -
 - The temporary file is created with 'openTempFile'. The directory is the
 - one indicated by 'getTemporaryDirectory', or if the system has no notion
 - of a temporary directory, "." is used.
 -
 - The given filename pattern is passed to 'openTempFile'.
 -
 - After the argument function terminates, even if it terminates due to an
 - exception, the Handle is closed and the file is deleted.
 -}
withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pattern func = do

  -- The library reference of 'getTemporaryDirectory' indicates that it may
  -- raise an exception on systems that have no notion of a temporary
  -- directory.
  --
  -- Thus we run 'getTemporaryDirectory' under 'catch'. Where 'catch' takes
  -- two functions:
  --   1) a function to execute
  --   2) a function which will get called if the first raises an exception
  --
  -- The second function passed to 'catch' will use "." as the temporary
  -- directory. (ie: if 'getTemporaryDirectory' throws an exception, "."
  -- (the current working directory) will be used)
  tempDir <- catch getTemporaryDirectory ((\_ -> return ".") :: IOException -> IO String)
  (tempFile, tempH) <- openTempFile tempDir pattern

  -- Call (func tempFile tempH) to perform the action on the temporary file.
  --
  -- 'finally' takes two actions:
  --   1) the action to run under normal circumstances
  --   2) another action to run regardless of whether the first aciton
  --      raises and exception
  --
  -- By closing the Handle and deleting the temp file in the second action,
  -- we can gurantee that the temporary file is removed at the end of this
  -- function
  finally (func tempFile tempH)
          (do hClose tempH
              removeFile tempFile)

{-
 - By using hPutStrLn instead of hPutStr, the data written to file is always
 - terminated by a '\n' char, as a result the position is from 'hTell' is
 - 1 + the number of bytes written in tempData.
 -
 - Since we added a newline, when printing out the results with hPutStrLn,
 - we end up printing two '\n' chars after the initial tempData, which is
 - why there is an empty blank line after the list of numbers of actual data.
 -
 - The '\n' char in the Haskel Literal representation is the result of the
 - '\n' saved into the file by our use of hPutStrLn
 -}
