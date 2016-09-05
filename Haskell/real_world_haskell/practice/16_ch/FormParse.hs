import Text.ParserCombinators.Parsec
import Numeric

{-
 - Then 'GenParsec' monad is an instance of monad plus as defined as
 - follows:
 -
 - instance MonadPlus (GenParser tok st) where
 -   mzero = fail "mzero"
 -   mplus = (<|>)
 -}

p_query :: CharParser () [(String, Maybe String)]
p_query = p_pair `sepBy` char '&'

{-
 - Note that 'optionMaybe' returning 'Nothing' for failed parse and parsed
 - value wrapped in 'Just' if successful
 -
 - Note that the below 'p_pair' stores a parsed value in a temporary
 - variable 'name' and returns the results later instead of only returning
 - the very last result
 -}
p_pair :: CharParser () (String, Maybe String)
p_pair = do
  name  <- many1 p_char
  value <- optionMaybe $ char '=' >> many p_char
  return (name, value)

p_char :: CharParser () Char
p_char = oneOf urlBaseChars
     <|> (char '+' >> return ' ')
     <|> p_hex

urlBaseChars :: String
urlBaseChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "$-_.!*'(),"

p_hex :: CharParser () Char
p_hex = do
  char '%'
  a <- hexDigit
  b <- hexDigit
  let ((d, _):_) = readHex [a, b]
  return . toEnum $ d

{-
 - Pointfree version of 'p_pair'
 -
 - This can be read as appling parsers, then combine their results.
 - ie: lift 'many1 p_char' into the left of the tuple then
 -     lift 'optionMaybe ...' into the right of the tuple
 -}
p_pair_app1 :: CharParser () (String, Maybe String)
p_pair_app1 = liftM2 (,) (many1 p_char) (optionMaybe $ char '=' >> many p_char)
