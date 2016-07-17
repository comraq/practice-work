import System.Environment
import Data.Char
import Data.List
import Control.Monad
import System.IO
import System.Directory

main2 = do  
  return ()  
  l2 <- return "HAHAHA"
  line <- getLine
  return "BLAH BLAH BLAH"
  return 4
  putStrLn $ line ++ l2

reverseWords :: String -> String  
reverseWords =  unwords . map reverse . words  

main3 = do
  c <- getChar
  if c /= ' '
    then do
      putChar c
      main3
    else
      return ()

main4 = do
  c <- getChar
  when (c /= ' ') $ do
    putChar c
    main4

main5 = do
  rs <- sequence [ getLine, getLine, getLine ]
  print rs

main6 = do
  colors <- forM [ 1, 2, 3, 4 ] (\a -> do
    putStrLn $ "Current number: " ++ show a ++ "?"
    getLine)
  putStrLn "Results: "
  mapM putStrLn colors

main7 = forever $ do
  putStr "Waiting for input: "
  line <- getLine
  putStrLn $ map toUpper line

main8 = do
  contents <- getContents
  putStr $ map toUpper contents

main9 = interact shortLinesOnly

shortLinesOnly   :: String -> String
shortLinesOnly s = o
  where o = unlines . (filter (\x -> length x < 10)) . lines $ s

main10 = interact respondPalindromes

respondPalindromes :: String -> String
respondPalindromes =  unlines . checkPali . lines
  where checkPali = let isPali x = x == reverse x in
                      map (\x -> x ++
                            (if isPali x
                               then " is a palindrome"
                               else " is not a palindrome"))

main11 = do
  fh <- openFile "dummy" ReadMode
  contents <- hGetContents fh
  putStr contents
  hClose fh

main12 = do
  withFile "dummy" ReadMode (\fh -> (hGetContents fh) >>= putStr)

withFile'       :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' p m f = do
  fh <- openFile p m
  res <- f fh
  hClose fh
  return res

main13 = do
  withFile "dummy" ReadMode (\fh -> do
    hSetBuffering fh $ BlockBuffering (Just 2048)
    contents <- hGetContents fh
    putStr contents)

main14 = do
  fh <- openFile "dummy" ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents fh
  let lns = lines contents
      ol  = zipWith (\n l -> show n ++ " - " ++ l) [0..] lns
  putStrLn "Lines from file:"
  mapM putStrLn ol
  putStrLn "Delete?"
  lineNum <- getLine
  let n = read lineNum
      newContents = delete (lns !! n) lns
  hPutStr tempHandle $ unlines newContents
  hClose fh
  hClose tempHandle
  removeFile "dummy"
  renameFile tempName "dummy"

dispatch :: [ ( String, [String] -> IO() ) ]
dispatch =  [ ( "add", add )
            , ( "view", view )
            , ( "remove", remove )
            ]

main = do
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch
  action args

add           :: [String] -> IO ()
add [ fn, e ] =  appendFile fn (e ++ "\n")

view        :: [String] -> IO ()
view [ fn ] =  do
  contents <- readFile fn
  let ul = lines contents
      ol = zipWith (\n l -> show n ++ " - " ++ l) [0..] ul
  putStr $ unlines ol

remove           :: [String] -> IO ()
remove [ fn, n ] =  do
  fh <- openFile fn ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents fh
  let lineNum = read n
      ul = lines contents
      newContents = delete (ul !! lineNum) ul
  hPutStr tempHandle $ unlines newContents
  hClose fh
  hClose tempHandle
  removeFile fn
  renameFile tempName fn
