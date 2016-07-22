import System.Environment
import System.Directory
import System.IO
import System.IO.Error
import Data.List

main1 = do (fn:_) <- getArgs
           contents <- readFile fn
           putStrLn $ "File has " ++ show (length $ lines contents) ++ " lines!"

main2 = do
  (fn:_) <- getArgs
  fExists <- doesFileExist fn
  if fExists
    then do contents <- readFile fn
            putStrLn $ "File has " ++ show (length $ lines contents) ++ " lines!"
    else do putStrLn "File does not exist!"

toTry :: IO ()
toTry =  do (fn:_) <-getArgs
            contents <- readFile fn
            putStrLn $ "File has " ++ show (length $ lines contents) ++ " lines!"

handler   :: IOError -> IO ()
handler e
  | isDoesNotExistError e =
    case ioeGetFileName e of
      Just path -> putStrLn $ "File for path: " ++ path ++ " does not exist!"
      Nothing   -> putStrLn "File does not exist! Got no path information!"

  | otherwise             = ioError e  -- Re-throw if not caught to avoid silent failures!

main = toTry `catchIOError` handler

