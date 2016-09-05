import Text.ParserCombinators.Parsec
import Numeric

-- Note that 'CharParser' is simply a type synonym for 'GenParser Char'

p_hex :: CharParser () Char
p_hex = do
  char '%'
  a <- hexDigit
  b <- hexDigit
  let ((d, _):_) = readHex [a, b]
  return . toEnum $ d

{-
 - Since '*>' only returns the value from the expression on the right,
 - 'hexify <$> (char '%' *> hexDigit)' is simply a parser that matches a '%'
 - character followed by any hex digit. The result is still a function that
 - needs to take in one more 'Char' before returning a 'Char'
 -
 - The final '<*> hexDigit' simply lifts the function inside the applicative
 - functor on the left to be applied to the result in the applicative
 - functor on the right, returning the final result wrapped within the
 - applicative functor.
 -
 - ex:
 - > char '%' *> hexDigit              :: GenParser Char st Char
 - > hexify <$> (char '%' *> hexDigit) :: GenParser Char st (Char -> Char)
 -
 - Note: ($), (<*>), 'ap' and 'app' are all comparable.
 -       ($)   ::                    (a -> b) ->   a ->   b
 -       (<*>) :: Applicative f => f (a -> b) -> f a -> f b
 -       ap    :: Monad m       => m (a -> b) -> m a -> m b
 -       app   :: ArrowApply a  => a (a b c -> c, b) ->   c
 -}
a_hex :: CharParser () Char
a_hex = hexify <$> (char '%' *> hexDigit) <*> hexDigit

hexify :: Char -> Char -> Char
hexify a b = toEnum . fst . head . readHex $ [a, b]

p_char :: CharParser () Char
p_char = oneOf urlBaseChars
     <|> (char '+' >> return ' ')
     <|> p_hex

urlBaseChars :: String
urlBaseChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "$-_.!*'(),"

a_char :: CharParser () Char
a_char = oneOf urlBaseChars
     <|> (' ' <$ char '+')
     <|> a_hex

p_pair_app1 :: CharParser () (String, Maybe String)
p_pair_app1 = liftM2 (,) (many1 p_char) (optionMaybe $ char '=' >> many p_char)

a_pair :: CharParser () (String, Maybe String)
a_pair = liftA2 (,) (many1 a_char) (optionMaybe $ char '=' *> many a_char)
