{-# LANGUAGE DeriveFunctor
           , DeriveFoldable
           , FlexibleContexts
  #-}

module JSONParser
  ( module Text.ParserCombinators.Parsec

  , JSValue(..)
  , JSValueF(..)

  , pJSValueF
  , parse'
  ) where


import Control.Applicative (empty)
import Numeric (readSigned, readFloat)
import Text.ParserCombinators.Parsec

-- Unfixed JSON data-type
data JSValueF r = JSNullF
                | JSBoolF   Bool
                | JSNumberF Double
                | JSStringF String
                | JSArrayF  [r]
                | JSObjectF [(String, r)]
  deriving (Show, Eq, Ord, Functor, Foldable)


parse' :: CharParser () a -> String -> a
parse' p = either (error . show) id . parse p "(unknown)"

pJSValueF :: CharParser () r -> CharParser () (JSValueF r)
pJSValueF r = spaces *> pValue r

pSeries :: Char -> CharParser () r -> Char -> CharParser () [r]
pSeries left parser right = between (char left <* spaces) (char right)
                          $ (parser <* spaces) `sepBy` (char ',' <* spaces)

pArray :: CharParser () r -> CharParser () [r]
pArray r = pSeries '[' r ']'

pObject :: CharParser () r -> CharParser () [(String, r)]
pObject r = pSeries '{' pField '}'
  where pField = (,) <$> (pString <* char ':' <* spaces) <*> r

pBool :: CharParser () Bool
pBool = True <$ string "true" <|> False <$ string "false"

pValue :: CharParser () r -> CharParser () (JSValueF r)
pValue r = value <* spaces
  where value = choice [ JSStringF <$> pString
                       , JSNumberF <$> pNumber
                       , JSObjectF <$> pObject r
                       , JSArrayF  <$> pArray  r
                       , JSBoolF   <$> pBool
                       , JSNullF   <$  string "null"
                       ]
                <?> "JSON value"

pNumber :: CharParser () Double
pNumber = do s <- getInput
             case readSigned readFloat s of
               [(n, s')] -> n <$ setInput s'
               _         -> empty

pString :: CharParser () String
pString = between (char '\"') (char '\"') (many jchar)
  where jchar = char '\\' *> pEscape <|> satisfy (`notElem` "\"\\")

pEscape = choice (zipWith decode "bnfrt\\\"/" "\b\n\f\r\t\\\"/")
  where decode c r = r <$ char c

-- Unparametrized JSValue type
data JSValue = JSNull
             | JSBool   Bool
             | JSNumber Double
             | JSString String
             | JSArray  [JSValue]
             | JSObject [(String, JSValue)]
  deriving Show
