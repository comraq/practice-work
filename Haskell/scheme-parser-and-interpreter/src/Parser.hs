module Parser (readExpr) where

import Text.ParserCombinators.Parsec

import Numeric (readHex, readOct, readInt, readFloat, readDec)
import Data.Char (digitToInt)
import Data.Maybe (maybeToList)
import Control.Monad (void)
import Data.Ratio
import Data.Complex

import Definition



readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err  -> LString $ "No match: " ++ show err
  Right val -> val



------- Parsers -------

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseNumber
        <|> parseChar
        <|> parseBool
        <|> parseString
        <|> parseAnyQuoted
        <|> parseAnyList


------- Any Atom Parser -------

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest  <- many $ letter <|> digit <|> symbol
  return . LAtom $ first : rest


------- Bool Parser -------

parseBool :: Parser LispVal
parseBool = try $ do
  char '#'
  boolChar <- oneOf "tf"
  return . LBool $ case boolChar of
    't' -> True
    'f' -> False


------- Character Literal Parser -------

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


------- String Parser -------

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


------- Number Parsers -------

parseNumber :: Parser LispVal
parseNumber = LNumber <$> parseSNumber

parseSNumber :: Parser SchemeNumber
parseSNumber = tryRational <|> tryComplex <|> noBase <|> try withBase
  where
    maybeNeg :: Num a => Parser (a -> a)
    maybeNeg = maybe id (const negate) <$> optionMaybe (char '-')

    toInt :: String -> Integer
    toInt = fst . head . readDec

    toDouble :: String -> Double
    toDouble = fst . head . readFloat

    getNumInt :: Parser Integer
    getNumInt = do
      mbNeg  <- maybeNeg
      digits <- many1 digit
      return . mbNeg . toInt $ digits

    getNumDouble :: Parser Double
    getNumDouble = do
      mbNeg  <- maybeNeg
      [a, b] <- take 2 <$> many1 digit `sepBy1` char '.'
      return . mbNeg . toDouble $ a ++ '.':b

    noBase :: Parser SchemeNumber
    noBase = SDouble <$> try getNumDouble <|> SInt <$> getNumInt

    withBase :: Parser SchemeNumber
    withBase = char '#' >> oneOf baseChars >>= getNumFromBaseChar

    baseChars :: String
    baseChars = "bodx"

    getNumFromBaseChar :: Char -> Parser SchemeNumber
    getNumFromBaseChar 'b' = SInt . fst . head . readBin   <$> many1 (oneOf binChars)
    getNumFromBaseChar 'o' = SInt . fst . head . readOct   <$> many1 octDigit
    getNumFromBaseChar 'd' = noBase
    getNumFromBaseChar 'x' = SInt . fst . head . readHex   <$> many1 hexDigit

    tryRational :: Parser SchemeNumber
    tryRational = try $ do
      [a, b] <- take 2 <$> getNumInt `sepBy1` char '/'
      return . SRational $ a % b

    tryComplex :: Parser SchemeNumber
    tryComplex = fmap SComplex $ rectComplex <|> polarComplex

    rectComplex :: Parser (Complex Double)
    rectComplex = try $ do
      rPart    <- getNum
      iPartNeg <- oneOf "+-" >>= \op -> return $ if op == '-'
                                          then negate
                                          else id

      iPart    <- iPartNeg <$> getNum
      char 'i'
      return $ rPart :+ iPart

    polarComplex :: Parser (Complex Double)
    polarComplex = try $ do
      mag   <- maybeNeg <*> getNumInt
      char '@'
      angle <- getNum
      return $ mkPolar (fromIntegral mag) angle

    getNum :: Parser Double
    getNum = try getNumDouble <|> fmap fromIntegral getNumInt


------- Quoted Parsers -------

parseAnyQuoted :: Parser LispVal
parseAnyQuoted = try parseQuasiQuoted <|> parseQuoted

parseQuoted :: Parser LispVal
parseQuoted = do
  oneOf "'`"
  x <- parseExpr
  return $ LList [LAtom "quote", x]

-- TODO: Add support for ',@' to unquote and evaluate lists
parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = between (string "`(") (char ')') quasiQuoted
  where
    quasiQuoted :: Parser LispVal
    quasiQuoted = LList . (LAtom "quasiquote":) <$> ((unquoted <|> parseExpr) `sepBy` spaces1)

    unquoted :: Parser LispVal
    unquoted = fmap (LList . (LAtom "unquoted":) . (:[]))
                  $ char ',' >> parseExpr



------- List Parsers -------

parseAnyList :: Parser LispVal
parseAnyList = between (char '(') (char ')') anyList
  where
    anyList :: Parser LispVal
    anyList = try parseList <|> parseDottedList

parseList :: Parser LispVal
parseList = LList <$> parseExpr `sepBy` spaces1

parseDottedList :: Parser LispVal
parseDottedList =
  let head = parseExpr `endBy` spaces1
      tail = char '.' >> spaces1 >> parseExpr
  in  LDottedList <$> head <*> tail


------- Helper Parsers -------

binChars :: String
binChars = "01"

-- http://stackoverflow.com/questions/5921573/convert-a-string-representing-a-binary-number-to-a-base-10-string-haskell
readBin :: String -> [(Integer, String)]
readBin = readInt 2 (`elem` binChars) digitToInt

symbol :: Parser Char
symbol =  oneOf "!$%&|*+/:<=?>@^_~"

spaces1 :: Parser ()
spaces1 =  skipMany1 space


------- Unused -------

parseNumber' :: Parser LispVal
parseNumber' =  LNumber . read <$> many1 digit

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
