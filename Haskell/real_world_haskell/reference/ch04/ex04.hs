import qualified Ch04 as C4
import System.Environment (getArgs)
import Data.Char (digitToInt, isDigit)
import Data.List (foldl')
import Data.Function (fix)

safeHead       :: [a] -> Maybe a
safeHead (x:_) =  Just x
safeHead _     =  Nothing

safeTail        :: [a] -> Maybe [a]
safeTail (x:xs) =  Just xs
safeTail _      =  Nothing

safeLast        :: [a] -> Maybe a
safeLast [x]    =  Just x
safeLast (x:xs) =  safeLast xs
safeLast _      =  Nothing

safeInit        :: [a] -> Maybe [a]
safeInit [x]    =  Just [x]
safeInit []     =  Nothing
safeInit (x:xs) =  maybe Nothing (\xs -> Just $ x:xs) (safeInit xs)

splitWith          :: (a -> Bool) -> [a] -> [[a]]
splitWith f (x:xs) =  case f x of
  False -> splitWith f xs
  True  -> foldr split [[]] (x:xs)
  where split x (xs:xss) = case f x of
          True  -> (x:xs):xss
          False -> if length xs == 0 then xs:xss else []:(xs:xss)

interactWith :: (String -> String) -> String -> String -> IO ()
interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

mainWith :: (String -> String) -> IO ()
mainWith function = do
        args <- getArgs
        case args of
          [input, output] -> interactWith function input output
          _               -> putStrLn "error: exactly two arguments needed"

withLine   :: (String -> String) -> IO ()
withLine f =  getLine >>= \x -> putStrLn $ f x

firstWord :: String -> String
firstWord =  head . words

main :: IO ()
main =  withFunc myFunction
  where withFunc = withLine
        myFunction = firstWord

type Filename = String

gather :: [[a]] -> [a]
gather =  foldr (\(x:xs) acc -> x:acc) []

trans      :: [[a]] -> [[a]]
trans xss  =  case any (\xs -> length xs == 0) xss of
  True  -> []
  False -> let ys   = gather xss
               xss' = map tail xss 
           in ys:(trans xss')

transpose :: String -> String
transpose = unlines . trans . lines

asInt        :: String -> Either String Int
asInt []     =  Right 0
asInt (c:cs)
  | c == '-'        = fmap (* (-1)) $ asInt cs
  | not $ isDigit c = Left $ c: " is not a digit!"
  | otherwise       = Right $ foldl' addDigit 0 (c:cs)
     where addDigit acc c = (acc * 10) + digitToInt c

concat' :: [[a]] -> [a]
concat' =  foldr (++) []

takeWhile'          :: (a -> Bool) -> [a] -> [a]
takeWhile' _ []     =  []
takeWhile' f (x:xs) =  case f x of
  True  -> x: (takeWhile' f xs)
  False -> []

tWFold      :: (a -> Bool) -> [a] -> [a]
tWFold f xs =  fst $ tw' f xs
  where tw' f xs = foldl' helper ([], xs) xs
        helper (acc, y:ys) _ = case f y of
          True  -> (acc ++ [y], ys)
          False -> (acc, y:ys)

groupBy          :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ []     =  []
groupBy f (x:xs) =
  let (ys, rest) = span (f x) xs
  in [x:ys] ++ groupBy f rest

gbFold      :: (a -> a -> Bool) -> [a] -> [[a]]
gbFold f xs =  (reverse . fst) $ gb' f xs
  where gb' f xs = foldl' helper ([[]], xs) xs
        helper (as:acc, y:ys) x = case f y x of
          True -> ((as ++ [x]):acc, y:ys)
          False -> ([x]:(as:acc), drop (length as - 1) ys)

anyFold   :: (a -> Bool) -> [a] -> Bool
anyFold f =  foldr check False
  where check _ True = True
        check x _    = f x

words' :: String -> [String]
words' =  splitWith (/= ' ')

unlines'          :: [String] -> String
unlines' (s:strs) =  s ++ foldl' (\xs x -> xs ++ ('\n':x)) "" strs
unlines' _        =  ""

cycle'    :: [a] -> [a]
cycle' xs =  xs' where xs' = xs ++ xs'
