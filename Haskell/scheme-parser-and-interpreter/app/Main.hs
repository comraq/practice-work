module Main where

import System.Environment
import Parser (readExpr)

------ Entry Point (Main) -------

main :: IO ()
main = do
  args <- getArgs
  putStrLn . readExpr . head $ args

mainI :: IO ()
mainI = do
  input <- getLine
  putStrLn $ readExpr input

mainI' :: String -> IO ()
mainI' = putStrLn . readExpr
