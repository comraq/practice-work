{-# LANGUAGE FlexibleInstances #-}

-- A relatively recent post on regex in Haskell
-- @link - http://gabebw.com/blog/2015/10/11/regular-expressions-in-haskell

module RunProcess where

import System.Process hiding (createPipe)
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception (catch, SomeException, evaluate)
import System.Directory (setCurrentDirectory, createDirectory)
import System.IO
import System.Exit
import Text.Regex.Posix
import System.Posix.Process
import System.Posix.IO
import System.Posix.Types
import Data.List
import System.Posix.Env (getEnv)

import RunProcessSimple

instance CommandLike String where
  invoke cmd closefds input = do
    {-
     - Use the shell given by the environment variable $SHELL, if any.
     - Otherwise, use "/bin/sh"
     -}
    esh <- getEnv "SHELL"
    let sh = maybe "/bin/sh" id esh
    invoke (sh, ["-c", cmd]) closefds input

-- Support for running Haskell commands
instance CommandLike (String -> IO String) where
  invoke func _ input = return $
    CommandResult (func input) (return $ Exited ExitSuccess)

-- Support pure Haskell functions by wrapping them in IO
instance CommandLike (String -> String) where
  invoke func = invoke iofunc
    where iofunc :: String -> IO String
          iofunc = return . func

{-
 - It is also useful to operate on lines. Define support for line-based
 - functions both within and without the IO Monad.
 -}
instance CommandLike ([String] -> IO [String]) where
  invoke func _ input = return $
      CommandResult linedfunc (return $ Exited ExitSuccess)

    where linedfunc = func (lines input) >>= return . unlines

instance CommandLike ([String] -> [String]) where
  invoke func = invoke (unlines . func . lines)


{-
 - Different ways to get data from 'run':
 -
 - * IO () runs, throws an exception on error, and sends 'stdout' to 'stdout'
 -
 - * IO String runs, throws an exception on error, reads 'stdout' into a
 -   buffer, and returns it as a string
 -
 - * IO [String] is same as IO String, but returns the results as line
 -
 - * IO ProcessStatus runs and returns a ProcessStatus with the exit
 -   information. 'stdout' is sent to 'stdout'. Exceptions are not thrown.
 -
 - * IO (String, ProcessStatus) is like IO ProcessStatus, but also includes
 -   a description of the last command in the pipe to have an error (or the
 -   last command, if there was no error)
 -
 - * IO Int returns the exit code from a program directly. If a signal
 -   caused the command ot be reaped, returns 128 + SIGNUM.
 -
 - * IO Bool returns True if the program exited normally (exit code 0, not
 -   stopped by a signal) or False otherwise.
 -}

class RunResult a where
  {-
   - Runs a command (or pipe of commands), with results presented in any
   - number of different ways
   -}
  run :: CommandLike b => b -> a

setUpCommand :: CommandLike a => a -> IO CommandResult
setUpCommand cmd = do
  -- Initialize our closefds list
  closefds <- newMVar []

  -- Invoke the command
  invoke cmd closefds []

instance RunResult (IO ()) where
  run cmd = run cmd >>= checkResult

instance RunResult (IO ProcessStatus) where
  run cmd = do
    res <- setUpCommand cmd

    -- Process its output
    output <- cmdOutput res
    putStr output

    getExitStatus res

instance RunResult (IO Int) where
  run cmd = do
    rc <- run cmd
    case rc of
      Exited ExitSuccess     -> return 0
      Exited (ExitFailure x) -> return x
      Terminated x _         -> return $ 128 + fromIntegral x
      Stopped x              -> return $ 128 + fromIntegral x

instance RunResult (IO Bool) where
  run cmd = do
    rc <- run cmd
    return $ (rc :: Int) == 0

instance RunResult (IO [String]) where
  run cmd = do
    r <- run cmd
    return $ lines r

instance RunResult (IO String) where
  run cmd = do
    res    <- setUpCommand cmd
    output <- cmdOutput res

    -- Force output to be buffered so child process can be safely closed after
    evaluate $ length output

    ec <- getExitStatus res
    checkResult ec
    return output

checkResult :: ProcessStatus -> IO ()
checkResult (Exited ExitSuccess) = return ()
checkResult x                    = fail $ show x

{-
 - A convenience function. Refers only to the version of 'run' that returns
 - 'IO ()'. This prevents you from having to cast to it all the time when
 - you do not care about the result of 'run'.
 -}
runIO' :: CommandLike a => a -> IO ()
runIO' = run


------- Utility Functions -------

cd :: FilePath -> IO ()
cd = setCurrentDirectory

{-
 - Takes a string and sends it on as standard output. The input to this
 - function is never read.
 -}
echo :: String -> String -> String
-- echo inp _ = inp
echo = const

-- Search for the regexp in the lines and return those that match
grep :: String -> [String] -> [String]
grep pat = filter (`ismatch` pat)
  where ismatch = (=~) :: String -> String -> Bool

{-
 - Creates the given directory. A avlue of 0o755 for mode would be typical.
 - An alias for System.Posix.Directory.createDirectory
 -}
mkdir :: FilePath -> IO ()
mkdir = createDirectory

{-
 - Remove duplicate lines from a file (liek Unix uniq).
 - Takes a String representing a file or output and plugs it thro8ugh lines
 - and then nub to uniqify on a line basis.
 -}
uniq :: String -> String
uniq = unlines . nub . lines

wcL, wcW :: [String] -> [String]

-- Count number of lines, ie: "wc -l"
wcL inp = [show (genericLength inp :: Integer)]

-- Count number of words in a file, ie: "wc -w"
wcW inp = [show ((genericLength . words . unlines $ inp) :: Integer)]

sortLines :: [String] -> [String]
sortLines = sort

-- Count the lines in the input
countLines :: String -> IO String
countLines = return . (++) "\n" . show . length . lines
