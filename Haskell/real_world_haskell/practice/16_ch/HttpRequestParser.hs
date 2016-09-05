module HttpRequestParser
  ( HttpRequest(..)
  , Method(..)
  , p_request
  , p_query
  ) where

import ApplicativeParsec
import Numeric (readHex)
import Control.Monad (liftM4)
import System.IO (Handle)

-- For simplicity, we will only define 2 valid HTTP Methods, 'GET' and 'POST'
data Method = Get | Post
  deriving (Eq, Ord, Show)

data HttpRequest = HttpRequest {
  reqMethod  :: Method
, reqURL     :: String
, reqHeaders :: [(String, String)]
, reqBody    :: Maybe String
} deriving (Eq, Show)

p_request :: CharParser () HttpRequest
p_request = q "GET" Get (pure Nothing)
        <|> q "POST" Post (Just <$> many anyChar)
  where
    q :: String -> Method -> CharParser () (Maybe String) -> CharParser () HttpRequest
    q name ctor body = liftM4 HttpRequest req url p_headers body
            -- Consumes spaces, then either 'GET or POST', finally returns
            -- the corresponding HTTP Method of the 'Method' type
      where req = ctor <$ string name <* char ' '

    url :: CharParser () String
    url = optional (char '/') *>
          manyTill notEOL (try $ string " HTTP/1." <* oneOf "01")
          <* crlf

{-
 - Note that the 'try' combinator has to hold onto input in case it needs to
 - restore it, so that an alternative parser can be used. This practice is
 - referred to as "backtracking". Because 'try' must save input, it is
 - expensive to use. Sprinkling a parser with unnecessary uses of 'try' is a
 - very effective way to slow it down, sometimes to the point of
 - unacceptable performance.
 -
 - The standard way to avoid the need for backtracking is to tidy up a
 - parser so that we can decide whether it will succeed or fail using only a
 - single token of input. In this case, the two parsers consume the same
 - initial tokens, so we turn them into a single parser.
 -
 - ex:
 -   > let parser = (++) <$> string "HT" <*> (string "TP" <|> string "ML")
 -
 -   > parserTest parser "HTTP"
 -   > "HTTP"
 -
 -   > parserTest parser "HTML"
 -   > "HTML"
 -}

p_headers :: CharParser st [(String, String)]
p_headers = header `manyTill` crlf
  where
    header :: CharParser st (String, String)
    header = liftA2 (,)  fieldName (char ':' *> spaces *> contents)

    contents :: CharParser st String
    contents = liftA2 (++) (many1 notEOL <* crlf)
                               (continuation <|> pure [])

    continuation :: CharParser st String
    continuation = liftA2 (:) (' ' <$ many1 (oneOf " \t")) contents

    fieldName :: CharParser st String
    fieldName = (:) <$> letter <*> many fieldChar

    fieldChar :: CharParser st Char
    fieldChar = letter <|> digit <|> oneOf "-_"

crlf :: CharParser st ()
crlf = (() <$ string "\r\n") <|> (() <$ newline)

notEOL :: CharParser st Char
notEOL = noneOf "\r\n"

p_query :: CharParser () [(String, Maybe String)]
p_query = pair `sepBy` char '&'
  where pair = (,) <$> many1 safe <*> optional (char '=' *> many safe)
        safe = oneOf urlBaseChars
           <|> char '%' *> liftA2 diddle hexDigit hexDigit
           <|> ' ' <$ char '+'
           <?> "safe"
        diddle a b = toEnum . fst . head . readHex $ [a,b]

urlBaseChars :: String
urlBaseChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "$-_.!*'(),"
