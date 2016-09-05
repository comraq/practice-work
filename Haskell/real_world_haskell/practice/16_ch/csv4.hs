import Text.ParserCombinators.Parsec

csvFile = line `endBy` eol
line    = cell `sepBy` char ','
cell    = many $ noneOf ",\n\r"

{-
 - Note that we do not need to surround 'string "\n"' or 'string "\r"' with
 - try because they only consume 1 character. And thus, consuming even just
 - the first character is all we need (no need for 'try')
 -}
eol = try (string "\n\r")
  <|> try (string "\r\n")
  <|> string "\n"
  <|> string "\r"
  <?> "end of line"

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input
