module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric (readHex, readOct, readInt)
import Data.Char (digitToInt)
import Control.Monad (void)



------- Type Definitions -------

data LispVal = LAtom       String
             | LList       [LispVal]
             | LDottedList [LispVal] LispVal
             | LNumber     Integer
             | LString     String
             | LBool       Bool
             | LChar       Char
  deriving (Show)


------ Entry Point (Main) -------

main :: IO ()
main = do
  args <- getArgs
  putStrLn . readExpr . head $ args

mainI :: IO ()
mainI = do
  input <- getLine
  putStrLn $ readExpr input

readExpr       :: String -> String
readExpr input =  case parse parseExpr "lisp" input of
  Left err  -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val

parseExpr :: Parser LispVal
parseExpr =  parseAtom <|> parseString <|> parseNumber' <|> parseChar <|> parseBool




------- Parsers -------

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest  <- many $ letter <|> digit <|> symbol
  return . LAtom $ first : rest

parseBool :: Parser LispVal
parseBool = do
  char '#'
  boolChar <- oneOf "tf"
  return . LBool $ case boolChar of
    't' -> True
    'f' -> False

parseChar :: Parser LispVal
parseChar = try $ do
  string "#\\"
  chr <- characterName <|> character
  return $ LChar chr

  where
    delimiters :: String
    delimiters = " ()"

    character :: Parser Char
    character = do
      chr <- anyChar
      void (oneOf delimiters) <|> eof
      return chr

    characterName :: Parser Char
    characterName = (string "space" <|> string "newline") >>= \chrName ->
      return $ case chrName of
        "space"   -> ' '
        "newline" -> '\n'

parseString :: Parser LispVal
parseString = do
  char '"'
  strings <- many validString
  char '"'
  return . LString . concat $ strings

validString :: Parser String
validString = many1 (noneOf "\\\"") <|> escaped
  where
    escaped :: Parser String
    escaped =  char '\\' >>
      oneOf "\\\"ntr" >>=
        \x -> return $ case x of
          'n' -> "\n"
          't' -> "\t"
          'r' -> "\r"
          _   -> [x]

parseNumber' :: Parser LispVal
parseNumber' = LNumber <$> parseNum
  where
    parseNum :: Parser Integer
    parseNum = noBase <|> try withBase

    noBase :: Parser Integer
    noBase = read <$> many1 digit

    withBase :: Parser Integer
    withBase = char '#' >> oneOf baseChars >>= getNumFromBaseChar

    baseChars :: String
    baseChars = "bodx"

    getNumFromBaseChar :: Char -> Parser Integer
    getNumFromBaseChar 'b' = fst . head . readBin <$> many1 (oneOf binChars)
    getNumFromBaseChar 'o' = fst . head . readOct <$> many1 (oneOf octChars)
    getNumFromBaseChar 'd' = (read :: String -> Integer) <$> many1 digit
    getNumFromBaseChar 'x' = fst . head . readHex <$> many1 (oneOf hexChars)

hexChars :: String
hexChars = ['0'..'9'] ++ ['A'..'F']

octChars :: String
octChars = ['0'..'7']

binChars :: String
binChars = "01"

-- http://stackoverflow.com/questions/5921573/convert-a-string-representing-a-binary-number-to-a-base-10-string-haskell
readBin :: String -> [(Integer, String)]
readBin = readInt 2 (`elem` binChars) digitToInt

symbol :: Parser Char
symbol =  oneOf "!$%&|*+-/:<=?>@^_~"

spaces :: Parser ()
spaces =  skipMany1 space




------- Unused -------

parseNumber :: Parser LispVal
parseNumber =  LNumber . read <$> many1 digit

parseNumberDo :: Parser LispVal
parseNumberDo =  do
  num <- many1 digit
  return . LNumber . read $ num

{-
 - The following is commented out to avoid linter warning
 -
 - parseNumberBind :: Parser LispVal
 - parseNumberBind =  many1 digit >>= return . LNumber . read
 -}

parseAtomBind :: Parser LispVal
parseAtomBind = (letter <|> symbol) >>=
  \first -> many (letter <|> digit <|> symbol) >>=
    \rest ->
      let atom = first : rest
      in return $ case atom of
        "#t" -> LBool True
        "#f" -> LBool False
        _    -> LAtom atom

parseStringBind :: Parser LispVal
parseStringBind =  char '"' >> many validString >>= \x -> char '"' >> return (LString $ concat x)

