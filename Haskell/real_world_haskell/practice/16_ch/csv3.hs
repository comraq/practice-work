import Text.ParserCombinators.Parsec

{-
 - This function is incorrect as both left and right matches "\n" as the
 - first sequence in the string.
 -   ie: The right match to the choice (<|>) combinator will never be called
 -
 - ex: both 'parse eol1 "" "\n"' and 'parse eol1 "" "\n\r"'
 -     only consumes the first '\n' and leaves the remaining string
 -     untouched
 -}
eol1 :: GenParser Char st String
eol1 = string "\n" <|> string "\n\r"

{-
 - Note that 'eol2' will still not be correct as (<|>) only attempts the
 - option on the right if the option on the left consumed no input. By the
 - time we are able to see if there is a '\r' after the '\n', we have
 - already consumed the '\n', thus we risk an 'Unexpected end of input'
 - error if we tried to parse a string such as "\n"
 -}
eol2 :: GenParser Char st String
eol2 = string "\n\r" <|> string "\n"

eolLookAhead :: GenParser Char st Char
eolLookAhead = do
  char '\n'
  char '\r' <|> return '\n'

