module Parser where

import Control.Monad
import qualified Control.Applicative as CA
import Data.Char (isSpace, isAlpha, isDigit, ord)
import Data.List (foldl')

import Ast

newtype Parser a = Parser (String -> [(a, String)])

instance Functor Parser where
  fmap f p = Parser $ \cs -> [ (f a, ss) | (a, ss) <- parse p cs ]

instance Applicative Parser where
  pure    = return
  f <*> a = f >>= \f' -> fmap f' a

instance Monad Parser where
  return a = Parser $ \cs -> [(a, cs)]
  p >>= f  = Parser $ \cs -> concat [ parse (f a) ss | (a, ss) <- parse p cs ]

parse            :: Parser a -> String -> [(a, String)]
parse (Parser p) =  p

{-
 - Parser combinator libraries usually includes:
 -
 - -------------------------- Simple Parsers ---------------------------
 -
­- The parser for a single character. The first available character (c)
­- is  passed and accepted.
­- As a result, a pair meaning “c was parsed, the rest is cs” will be
­- returned by the function hidden in the monadic capsule.
 -}

item :: Parser Char
item =  Parser $ \xs -> case xs of
                          ""     -> []
                          (c:cs) -> [(c, cs)]

{-
 - An operator needed to deal with multiple parsing possibilities
 -
 - It is simulating a parallel use of two parsers which will return a list
 - containing both set of results. Algebrists had noticed that Parser is
 - actually a MonadPlus, a monad which have a "zero" and a "plus". "mzero"
 - and "mplus" are respecting the appropriate algebraic rules.
 -}

instance CA.Alternative Parser where
  (<|>) = mplus
  empty = mzero

instance MonadPlus Parser where
  mzero = Parser $ \cs -> []
  p `mplus` q = Parser $ \cs -> parse p cs ++ parse q cs

{-
 - the given string of chars, cs is parsed by both and concatenated together
 - in the result
 -}

-- Due to the non-deterministic nature of list monoidal multiplication,
-- only retrieve the first parsed choice from result
(+++)   :: Parser a -> Parser a -> Parser a
p +++ q =  Parser $ \cs -> case parse (p `mplus` q) cs of
                             []     -> []
                             (x:xs) -> [x]

sat   :: (Char -> Bool) -> Parser Char
sat p =  do
            c <- item
            if p c
              then return c
              else mzero

infix 7 ?
(?)      :: Parser a -> (a -> Bool) -> Parser a
p ? test =  do
               b <- p
               if test b
                 then return b
                 else mzero

char   :: Char -> Parser Char
char c =  sat (c ==)

string        :: String -> Parser String
string ""     =  return ""
string (c:cs) =  do
                    char c
                    string cs
                    return $ c:cs

many   :: Parser a -> Parser [a]
many p =  many1 p +++ return []

many1   :: Parser a -> Parser [a]
many1 p =  do
              a <- p
              as <- many p
              return $ a:as

space :: Parser String
space =  many (sat isSpace)

token   :: Parser a -> Parser a
token p =  do
              a <- p
              space
              return a

symbol :: String -> Parser String
symbol =  token . string

apply   :: Parser a -> String -> [(a, String)]
apply p =  parse $ do
                      space
                      p

-- Parsers for Variables

-- Parser for identitiers
ident :: Parser String
ident =  do
            l   <- sat isAlpha
            lsc <- many $ sat $ \c -> isAlpha c || isDigit c
            return $ l:lsc

--- Parse the space after identifiers
identif :: Parser String
identif =  token ident

-- Parser for Variables
var :: Parser Exp
var =  do
          v <- identif
          return $ Variable v

-- Rules of the grammar
chain1        :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chain1 p op a =  (p `chainl1` op) +++ return a

chainl1        :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op =  do
                     a <- p
                     rest a
  where rest a = do
                    f <- op
                    b <- p
                    rest $ f a b
                 +++ return a

-- Parsers for one and more than one digits

-- Single Digit Parser
digit :: Parser Exp
digit =  do
            x <- token $ sat isDigit
            return $ Constant (ord x - ord '0')

-- Parser for unsigned number
digiti :: Parser Exp
digiti =  do
             p <- digit
             l <- many digit
             return $ foldl'
               (\a b -> let Constant nra = a
                            Constant nrb = b
                        in  Constant (10 * nra + nrb))
               (Constant 0)
               (p:l)

-- Parser for expressions and parts

-- Parsers for Expressions

rexp :: Parser Exp
rexp =  expr `chainl1` relop

expr :: Parser Exp
expr =  term `chainl1` addop

term :: Parser Exp
term =  factor `chainl1` mulop

-- Parser for factor, where factor ::= var | digiti | (expr)
factor :: Parser Exp
factor =  var +++
          digiti +++
          do
             symbol "("
             n <- rexp
             symbol ")"
             return n

-- Parsers for the various operators

addop :: Parser (Exp -> Exp -> Exp)
addop =  do
            symbol "-"
            return Minus
         +++
         do
            symbol "+"
            return Plus

mulop :: Parser (Exp -> Exp -> Exp)
mulop =  do
            symbol "*"
            return Times
         +++
         do
            symbol "/"
            return Div

relop :: Parser (Exp -> Exp -> Exp)
relop =  do
            symbol ">"
            return Greater
         +++
         do
            symbol "<"
            return Less
         +++
         do
            symbol "="
            return Equal

{-
 - Parsers for Commands
 -
 - Never forget the following:
 - - This kind of parsers did not consume the blanks from the beginning of
 -   the text
 - - A special function which is applied before parsing should deal with
 -   this kind of blanks or tabs
 -}

printe :: Parser Com
printe =  do
             symbol "print"; x <- rexp
             return $ Print x

assign :: Parser Com
assign =  do
             x <- identif; symbol ":="; e <- rexp
             return $ Assign x e

seqv :: Parser Com
seqv =  do
           symbol "{"; c <- com
           symbol ";"; d <- com
           symbol "}"
           return $ Seq c d

cond :: Parser Com
cond =  do
           symbol "if"; e <- rexp
           symbol "then"; c <- com
           symbol "else"; d <- com
           return $ Cond e c d

while :: Parser Com
while =  do
            symbol "while"; e <- rexp
            symbol "do"
            c <- com
            return $ While e c

declare :: Parser Com
declare =  do
              symbol "declare"; x <- identif;
              symbol "="; e <- rexp
              symbol "in"; c <- com;
              return $ Declare x e c

{-
 - A command can be either recognized by one of the following parsers:
 - - assign, seqv, cond, while, declare, printe
 -
 - Note: that +++ is used in place of |
 -
 - com ::= assign | seqv | cond | while | declare | printe
 -}

com :: Parser Com
com =  assign +++ seqv +++ cond +++ while +++ declare +++ printe

rawS1 :: String
rawS1 = "declare x = 150 in "
        ++ "declare y = 200 in "
           ++ "{while x > 0 do { x:=x-1; y:=y-1 }; "
              ++ "print y "
           ++ "}"

-- parse com rawS1
--
--   ==
--
-- [(Declare "x" (Constant 150) (Declare "y" (Constant 200) (Seq (While
-- (Greater (Variable "x") (Constant 0)) (Seq (Assign "x" (Minus (Variable
-- "x") (Constant 1))) (Assign "y" (Minus (Variable "y") (Constant 1)))))
-- (Print (Variable "y")))),"")]
