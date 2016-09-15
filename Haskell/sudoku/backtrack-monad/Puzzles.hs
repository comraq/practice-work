module Puzzles where

import Sudoku

type Grid = [[Value]]

h1 :: [[Value]]
h1 = [ "4.....8.5"
     , ".3......."
     , "...7....."
     , ".2.....6."
     , "....8.4.."
     , "....1...."
     , "...6.3.7."
     , "5..2....."
     , "1.4......"
     ]

gridToPuzzle :: Grid -> Puzzle
gridToPuzzle = map $ map valueToMaybe

valueToMaybe :: Value -> Maybe Value
valueToMaybe val
    | val `elem` values = Just val
    | otherwise         = Nothing
  where
    values :: [Value]
    values = getValues size
