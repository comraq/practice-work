module AParser where

import System.IO
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data BExpr = BoolConst Bool
           | Not BExpr
           | BBinary BBinOp BExpr BExpr
           | RBinary RBinOp AExpr AExpr
  deriving Show

data BBinOp = And | Or
  deriving Show

data RBinOp = Greater | Less
  deriving Show

data AExpr = Var String
           | IntConst Integer
           | Neg AExpr
           | ABinary ABinOp AExpr AExpr
  deriving Show

data ABinOp = Add
            | Subtract
            | Multiply
            | Divide
  deriving Show

data Stmt = Seq [Stmt]
          | Assign String AExpr
          | If BExpr Stmt Stmt
          | While BExpr Stmt
          | Skip
  deriving Show

languageDef :: LanguageDef ()
languageDef = emptyDef {
  Token.commentStart    = "/*"
, Token.commentEnd      = "*/"
, Token.commentLine     = "//"
, Token.identStart      = letter
, Token.identLetter     = alphaNum
, Token.reservedNames   = [ "if"
                          , "then"
                          , "else"
                          , "while"
                          , "do"
                          , "skip"
                          , "true"
                          , "false"
                          , "not"
                          , "and"
                          , "or"
                          ]
, Token.reservedOpNames = [ "+"
                          , "-"
                          , "*"
                          , "/"
                          , ":="
                          , "<"
                          , ">"
                          , "and"
                          , "or"
                          , "not"
                          ]
}

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser languageDef

identifier, semi :: Parser String
identifier = Token.identifier lexer -- parses an identifier
semi       = Token.semi lexer       -- parses a semicolon

reserved, reservedOp :: String -> Parser ()
reserved   = Token.reserved lexer   -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator

parens :: Parser a -> Parser a
parens = Token.parens lexer     -- takes a parser 'p', parses surrounding parenthesis
                                -- and returns the value of applying 'p'

integer :: Parser Integer
integer = Token.integer lexer    -- parses an integer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer -- parses white spaces

whileParser :: Parser Stmt
whileParser = whiteSpace >> statement

statement :: Parser Stmt
statement = parens statement <|> sequenceOfStmt

sequenceOfStmt :: Parser Stmt
sequenceOfStmt = do
  list <- statement' `sepBy1` semi

  -- If there is only one statement, return it without using 'Seq'
  return $ if length list == 1
             then head list
             else Seq list

statement' :: Parser Stmt
statement' =   ifStmt
           <|> whileStmt
           <|> skipStmt
           <|> assignStmt

ifStmt :: Parser Stmt
ifStmt = do
  reserved "if"
  cond  <- bExpression
  reserved "then"
  stmt1 <- statement
  reserved "else"
  stmt2 <- statement
  return $ If cond stmt1 stmt2

whileStmt :: Parser Stmt
whileStmt = do
  reserved "while"
  cond <- bExpression
  reserved "do"
  stmt <- statement
  return $ While cond stmt

assignStmt :: Parser Stmt
assignStmt = do
  var  <- identifier
  reservedOp ":="
  expr <- aExpression
  return $ Assign var expr

skipStmt :: Parser Stmt
skipStmt = reserved "skip" >> return Skip

aExpression :: Parser AExpr
aExpression = buildExpressionParser aOperators aTerm

bExpression :: Parser BExpr
bExpression = buildExpressionParser bOperators bTerm

{-
 - Note that the operator precedence depends on the order of the elements in
 - the below lists
 -}
aOperators :: OperatorTable Char () AExpr
aOperators = [ [ Prefix (reservedOp "-" >> return  Neg              )            ]
             , [ Infix  (reservedOp "*" >> return (ABinary Multiply)) AssocLeft,
                 Infix  (reservedOp "/" >> return (ABinary Divide  )) AssocLeft  ]
             , [ Infix  (reservedOp "+" >> return (ABinary Add     )) AssocLeft,
                 Infix  (reservedOp "-" >> return (ABinary Subtract)) AssocLeft  ]
             ]

bOperators :: OperatorTable Char () BExpr
bOperators = [ [ Prefix (reservedOp "not" >> return  Not            )            ]
             , [ Infix  (reservedOp "and" >> return (BBinary And   )) AssocLeft,
                 Infix  (reservedOp "or"  >> return (BBinary Or    )) AssocLeft  ]
             ]

aTerm :: Parser AExpr
aTerm =   parens aExpression
      <|> fmap Var identifier
      <|> fmap IntConst integer

bTerm :: Parser BExpr
bTerm =   parens bExpression
      <|> (reserved "true"  >> return (BoolConst True))
      <|> (reserved "false" >> return (BoolConst False))
      <|> rExpression

rExpression :: Parser BExpr
rExpression = do
  a1 <- aExpression
  op <- relation
  a2 <- aExpression
  return $ RBinary op a1 a2

relation :: Parser RBinOp
relation =   (reservedOp ">" >> return Greater)
         <|> (reservedOp "<" >> return Less)

parseString :: String -> Stmt
parseString str = case parse whileParser "" str of
  Left e  -> error $ show e
  Right r -> r

parseFile :: String -> IO Stmt
parseFile file = parseString <$> readFile file
