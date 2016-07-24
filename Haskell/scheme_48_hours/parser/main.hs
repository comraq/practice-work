module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad (liftM)

symbol :: Parser Char
symbol =  oneOf "!$%&|*+-/:<=?>@^_~#"

readExpr       :: String -> String
readExpr input =  case parse parseExpr "lisp" input of
  Left err  -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val

main :: IO ()
main =  do
  args <- getArgs
  putStrLn (readExpr (args !! 0))

spaces :: Parser ()
spaces =  skipMany1 space

data LispVal = LAtom String
             | LList [LispVal]
             | LDottedList [LispVal] LispVal
             | LNumber Integer
             | LString String
             | LBool Bool
  deriving (Show)

parseString :: Parser LispVal
parseString =  char '"' >> many (validString) >>= \x -> char '"' >> (return $ LString $ concat x)
  where validString = many1 (noneOf "\\\"") <|> escaped

escaped :: Parser String
escaped =  char '\\' >>
  oneOf "\\\"ntr" >>=
    \x -> return $ case x of
      'n'       -> "\n"
      't'       -> "\t"
      'r'       -> "\r"
      otherwise -> [x]
    

parseAtom :: Parser LispVal
parseAtom =  (letter <|> symbol) >>=
  \first -> many (letter <|> digit <|> symbol) >>=
    \rest ->
      let atom = first:rest
      in return $ case atom of
        "#t"      -> LBool True
        "#f"      -> LBool False
        otherwise -> LAtom atom

parseNumber :: Parser LispVal
parseNumber =  liftM (LNumber . read) $ many1 digit

parseNumberDo :: Parser LispVal
parseNumberDo =  do
  num <- many1 digit
  return . LNumber . read $ num

parseNumber' :: Parser LispVal
parseNumber' =  many1 digit >>= return . LNumber . read

parseExpr :: Parser LispVal
parseExpr =  parseAtom <|> parseString <|> parseNumber
