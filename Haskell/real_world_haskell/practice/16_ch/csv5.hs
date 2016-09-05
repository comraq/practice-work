import Text.ParserCombinators.Parsec

csvFile = line `endBy` eol
line    = cell `sepBy` char ','
cell    = quotedCell <|> many (noneOf ",\n\r")

quotedCell = do
  char '"'
  content <- many quotedChar
  char '"' <?> "quote at end of cell"
  return content

{-
 - Note that the although 'try' usually appears on the left side of (<|>),
 - in 'quotedChar' it appears on the right side. This is because in its
 - expanded usage it will be on the left side of a (<|>) (it is used in
 - 'quotedCell', which itself is used in the left side (<|>) of 'cell')
 -
 - Note that this 'try' is also necessary. As when parsing a quoted cell,
 - and is reaching towards the end. There will be another cell following. So
 - we will expect to see a quote to end the cell, followed by a comma. When
 - entering 'quotedChar', the 'noneOf' test will fail and proceed to look
 - for two quotes in a row. However this will also fail because the actual
 - text to parse contains one quote and a comma. Thus without 'try', the
 - parser would crash with an error as it consumed the first quote but
 - cannot continue due to the remaining comma.
 -}
quotedChar = noneOf "\""
         <|> try (string "\"\"" >> return '"')


eol = try (string "\n\r")
  <|> try (string "\r\n")
  <|> string "\n"
  <|> string "\r"
  <?> "end of line"

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

main :: IO ()
main = do
  c <- getContents
  case parse csvFile "(stdin)" c of
    Left e  -> do putStrLn "Error parsing input:"
                  print e
    Right r -> mapM_ print r
