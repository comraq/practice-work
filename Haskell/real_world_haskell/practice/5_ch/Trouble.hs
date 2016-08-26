import Data.Char (toUpper)

upcaseFirst (c:cs) = (toUpper c):cs

camelCase :: String -> String
camelCase = concat . (map upcaseFirst) . words
