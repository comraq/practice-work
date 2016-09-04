import Text.ParserCombinators.Parsec

csvFile = line `endBy` eol
line    = cell `sepBy` char ','
cell    = many $ noneOf ",\n"
eol     = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input
